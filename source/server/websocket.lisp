;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2014 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(define-constant +websocket-magic-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=
  :documentation "Fixed magic WebSocket UUIDv4 key use in handshakes")

(define-constant +continuation-frame+    #x0)
(define-constant +text-frame+            #x1)
(define-constant +binary-frame+          #x2)
(define-constant +connection-close+      #x8)
(define-constant +ping+                  #x9)
(define-constant +pong+                  #xA)

(defun control-frame-p (opcode)
  (plusp (logand #x8 opcode)))

;;; Mandatory API
;;;
(def (class* e) websocket-client ()
  ((input-stream (error "websocket-client must have an input-stream"))
   (output-stream (error "websocket-client must have an output-stream"))
   (write-lock (make-lock))
   (state :disconnected)
   (pending-fragments nil)
   (pending-opcode nil)))

(defmethod initialize-instance :after ((client websocket-client)
                                       &key &allow-other-keys)
  "Allows CLIENT to be passed more keywords on MAKE-INSTANCE.")

(def (class* e) websocket-broker (broker-at-path)
  ((clients nil)
   (client-class 'websocket-client)
   (lock (make-lock))))

(def print-object websocket-broker
  (format t "Path: ~S Priority:~S ~A connected clients of class ~A"
          (join-strings (path-of -self-) #\/)
          (priority-of -self-)
          (length (clients-of -self-))
          (client-class-of  -self-)))

(def (generic e) websocket-message-received (broker client message-type message)
  (:documentation "Called when a message is received through an already established websocket connection.
message-type is :text or :binary"))

;; Optional API
;;
(defgeneric client-connected (broker client)
  (:method (broker client)
    (declare (ignore broker client))))

(defgeneric client-disconnected (broker client)
  (:method (broker client)
    (declare (ignore broker client))))

(defgeneric check-message (broker client opcode fragment-length total-length)
  (:method ((broker websocket-broker)
            (client websocket-client) opcode length total)
    (declare (ignore broker client))
    (cond ((> length #xffff) ; 65KiB
           (websocket-error 1009 "Message fragment too big"))
          ((> total #xfffff) ; 1 MiB
           (websocket-error 1009 "Total message too big"))))
  (:method ((broker websocket-broker)
            (client websocket-client)
            (opcode (eql +binary-frame+)) length total)
    (websocket-error 1003 "Binaries not accepted")))

;; Convenience API
;;
(def (function e) send-text-message (client message)
  "MESSAGE is a string"
  (send-frame client +text-frame+
              (flexi-streams:string-to-octets message
                                              :external-format :utf-8)))

(defun close-connection (client &key data status reason)
  (send-frame client
              +connection-close+
              (or data
                  (concatenate 'vector
                               (coerce (list (logand (ash status -8) #xff)
                                             (logand status #xff))
                                       'vector)
                               (flexi-streams:string-to-octets
                                reason
                                :external-format :utf-8)))))


(defun send-frame (client opcode data)
  (with-slots (write-lock output-stream) client
    (with-lock-held (write-lock)
      (write-frame output-stream opcode data))))

(def (function ie) connected? (client)
  (eq :connected (state-of client)))

;;; Request/reply Hunchentoot overrides
;;;
(def class* websocket-request (request)
  ((broker nil :documentation "Broker of the current request (of type websocket-broker)")))

(defclass websocket-reply (http-response) ())

(defmethod initialize-instance :after ((reply websocket-reply) &rest initargs)
  "Set the reply's external format to Unix EOL / UTF-8 explicitly."
  (declare (ignore initargs))
  (setf (external-format-of reply) +default-external-format+))

;;; Conditions

(define-condition websocket-error (simple-error)
  ((error-status :initarg :status :reader websocket-error-status))
  (:documentation "Superclass for all errors related to Websocket."))

(defun websocket-error (status format-control &rest format-arguments)
  "Signals an error of type HUNCHENTOOT-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'websocket-error
         :status status
         :format-control format-control
         :format-arguments format-arguments))


;;; Client and broker machinery
;;;
(defmethod initialize-instance :after ((broker websocket-broker)
                                       &key client-class)
  (assert (subtypep client-class 'websocket-client)))

(defun call-with-new-client-for-broker (client broker fn)
  (with-slots (clients lock) broker
    (unwind-protect
         (progn
           (bt:with-lock-held (lock)
             (push client clients))
           (setf (slot-value client 'state) :connected)
           (client-connected broker client)
           (funcall fn))
      (bt:with-lock-held (lock)
        (with-slots (write-lock) client
          (bt:with-lock-held (write-lock)
            (setq clients (remove client clients))
            (setq write-lock nil))))
      (client-disconnected broker client))))

(defmacro with-new-client-for-broker ((client-sym &key input-stream
                                                  output-stream
                                                  broker)
                                      &body body)
  (alexandria:once-only (broker)
    `(let ((,client-sym (apply #'make-instance
                               (slot-value ,broker 'client-class)
                               'input-stream ,input-stream
                               'output-stream ,output-stream
                               #+nil(loop for (header . value)
                                          in (headers-of *request*)
                                          collect header collect value))))
       (call-with-new-client-for-broker ,client-sym
                                        ,broker
                                        #'(lambda () ,@body)))))

(defun websocket-uri (request host &optional ssl)
  "Form WebSocket URL (ws:// or wss://) URL."
  (format nil "~:[ws~;wss~]://~a~a" ssl host (path-of (uri-of request))))


;;; Binary reading/writing machinery
;;;
(defun read-unsigned-big-endian (stream n)
  "Read N bytes from stream and return the big-endian number"
  (loop for i from (1- n) downto 0
        sum (* (read-byte stream) (expt 256 i))))

(defun read-n-bytes-into-sequence (stream n)
  "Return an array of N bytes read from stream"
  (let* ((array (make-array n :element-type '(unsigned-byte 8)))
         (read (read-sequence array stream)))
    (assert (= read n) nil
            "Expected to read ~a bytes, but read ~a" n read)
    array))

(defclass websocket-frame ()
  ((opcode          :initarg :opcode :accessor frame-opcode)
   (data                             :accessor frame-data)
   (finp            :initarg :finp)
   (payload-length  :initarg :payload-length :accessor frame-payload-length)
   (masking-key     :initarg :masking-key)))

(defun read-frame (stream &key read-payload-p)
  (let* ((first-byte       (read-byte stream))
         (fin              (ldb (byte 1 7) first-byte))
         (extensions       (ldb (byte 3 4) first-byte))
         (opcode           (ldb (byte 4 0) first-byte))
         (second-byte      (read-byte stream))
         (mask-p           (plusp (ldb(byte 1 7) second-byte)))
         (payload-length   (ldb (byte 7 0) second-byte))
         (payload-length   (cond ((<= 0 payload-length 125)
                                  payload-length)
                                 (t
                                  (read-unsigned-big-endian
                                   stream (case payload-length
                                            (126 2)
                                            (127 8))))))
         (masking-key      (if mask-p (read-n-bytes-into-sequence stream 4)))
         (extension-data   nil))
    (declare (ignore extension-data))
    (when (and (control-frame-p opcode)
               (> payload-length 125))
      (websocket-error
       1002 "Control frame is too large" extensions))
    (when (plusp extensions)
      (websocket-error
       1002 "No extensions negotiated, but client sends ~a!" extensions))
    (let ((frame
            (make-instance 'websocket-frame :opcode opcode
                                  :finp (plusp fin)
                                  :masking-key masking-key
                                  :payload-length payload-length)))
      (when (or (control-frame-p opcode)
                read-payload-p)
        (read-application-data stream frame))
      frame)))

(defun read-frame-from-client (client)
  "Read a text or binary message from CLIENT."
  (with-slots (input-stream) client
    (read-frame input-stream)))

(defun mask-unmask (data masking-key)
  ;; RFC6455 Masking
  ;;
  ;; Octet i of the transformed data
  ;; ("transformed-octet-i") is the XOR of octet i
  ;; of the original data ("original-octet-i")
  ;; with octet at index i modulo 4 of the masking
  ;; key ("masking-key-octet-j"):
  (loop for i from 0 below (length data)
        do (setf (aref data i)
                 (logxor (aref data i)
                         (aref masking-key
                               (mod i 4)))))
  data)

(defun read-application-data (stream frame)
  (with-slots (masking-key payload-length data) frame
    (setq data (read-n-bytes-into-sequence stream
                                           payload-length))
    (when masking-key
      (mask-unmask data masking-key))))

(defun write-frame (stream opcode &optional data)
  (let* ((first-byte     #x00)
         (second-byte    #x00)
         (len            (if data (length data) 0))
         (payload-length (cond ((< len 125)         len)
                               ((< len (expt 2 16)) 126)
                               (t                   127)))
         (mask-p         nil))
    (setf (ldb (byte 1 7) first-byte)  1
          (ldb (byte 3 4) first-byte)  0
          (ldb (byte 4 0) first-byte)  opcode
          (ldb (byte 1 7) second-byte) (if mask-p 1 0)
          (ldb (byte 7 0) second-byte) payload-length)
    (write-byte first-byte stream)
    (write-byte second-byte stream)
    (loop for i from  (1- (cond ((= payload-length 126) 2)
                                ((= payload-length 127) 8)
                                (t                      0)))
          downto 0
          for out = (ash len (- (* 8 i)))
          do (write-byte (logand out #xff) stream))
    ;; (if mask-p
    ;;     (error "sending masked messages not implemented yet"))
    (if data (write-sequence data stream))
    (force-output stream)))


;;; State machine and main websocket loop
;;;
(defun handle-frame (broker client frame)
  (with-slots (state pending-fragments pending-opcode input-stream) client
    (with-slots (opcode finp payload-length masking-key) frame
      (flet ((maybe-accept-data-frame ()
               (check-message broker client (or pending-opcode
                                                  opcode)
                              payload-length
                              (+ payload-length
                                 (reduce #'+ (mapcar
                                              #'frame-payload-length
                                              pending-fragments))))
               (read-application-data input-stream frame)))
        (cond
          ((eq :awaiting-close state)
           ;; We're waiting a close because we explicitly sent one to the
           ;; client. Error out if the next message is not a close.
           ;;
           (unless (eq opcode +connection-close+)
             (websocket-error
              1002 "Expected connection close from client, got 0x~x" opcode))
           (setq state :closed))
          ((not finp)
           ;; This is a non-FIN fragment Check opcode, append to client's
           ;; fragments.
           ;;
           (cond ((and (= opcode +continuation-frame+)
                       (not pending-fragments))
                  (websocket-error
                   1002 "Unexpected continuation frame"))
                 ((control-frame-p opcode)
                  (websocket-error
                   1002 "Control frames can't be fragmented"))
                 ((and pending-fragments
                       (/= opcode +continuation-frame+))
                  (websocket-error
                   1002 "Not discarding initiated fragment sequence"))
                 (t
                  ;; A data frame, is either initiaing a new fragment sequence
                  ;; or continuing one
                  ;;
                  (maybe-accept-data-frame)
                  (cond ((= opcode +continuation-frame+)
                         (push frame pending-fragments))
                        (t
                         (setq pending-opcode opcode
                               pending-fragments (list frame)))))))
          ((and pending-fragments
                (not (or (control-frame-p opcode)
                         (= opcode +continuation-frame+))))
           ;; This is a FIN fragment and (1) there are pending fragments and (2)
           ;; this isn't a control or continuation frame. Error out.
           ;;
           (websocket-error
            1002 "Only control frames can interleave fragment sequences."))
          (t
           ;; This is a final, FIN fragment. So first read the fragment's data
           ;; into the `data' slot.
           ;;
           (cond
             ((not (control-frame-p opcode))
              ;; This is either a single-fragment data frame or a continuation
              ;; frame. Join the fragments and keep on processing. Join any
              ;; outstanding fragments and process the message.
              ;;
              (maybe-accept-data-frame)
              (unless pending-opcode
                (setq pending-opcode opcode))
              (let ((ordered-frames
                      (reverse (cons frame pending-fragments))))
                (cond ((eq +text-frame+ pending-opcode)
                       ;; A text message was received
                       ;;
                       (websocket-message-received
                        broker client :text
                        (flexi-streams:octets-to-string
                         (apply #'concatenate 'vector
                                (mapcar #'frame-data
                                        ordered-frames))
                         :external-format :utf-8)))
                      ((eq +binary-frame+ pending-opcode)
                       ;; A binary message was received
                       ;;
                       (let ((temp-file
                               (fad:with-output-to-temporary-file
                                   (fstream :element-type '(unsigned-byte 8))
                                 (loop for fragment in ordered-frames
                                       do (write-sequence (frame-data frame)
                                                          fstream)))))
                         (unwind-protect
                              (websocket-message-received broker client :binary
                                                       temp-file)
                           (delete-file temp-file))))
                      (t
                       (websocket-error
                        1002 "Client sent unknown opcode ~a" opcode))))
              (setf pending-fragments nil))
             ((eq +ping+ opcode)
              ;; Reply to client-initiated ping with a server-pong with the
              ;; same data
              (send-frame client +pong+ (frame-data frame)))
             ((eq +connection-close+ opcode)
              ;; Reply to client-initiated close with a server-close with the
              ;; same data
              ;;
              (close-connection client :data (frame-data frame))
              (setq state :closed))
             ((eq +pong+ opcode)
              ;; Probably just a heartbeat, don't do anything.
              )
             (t
              (websocket-error
               1002 "Client sent unknown opcode ~a" opcode)))))))))

(defun read-handle-loop (broker client
                         &optional (version :rfc-6455))
  "Implements the main WebSocket loop for supported protocol
versions. Framing is handled automatically, CLIENT handles the actual
payloads."
  (ecase version
    (:rfc-6455
     (handler-bind ((websocket-error
                      #'(lambda (error)
                          (with-slots (status format-control format-arguments)
                              error
                            (close-connection
                             client
                             :status status
                             :reason (princ-to-string error)))))
                    (flexi-streams:external-format-error
                      #'(lambda (e)
                          (declare (ignore e))
                          (close-connection client :status 1007
                                                   :reason "Bad UTF-8")))
                    (error
                      #'(lambda (e)
                          (declare (ignore e))
                          (close-connection client :status 1011
                                                   :reason "Internal error"))))
       (with-slots (state) client
         (loop do (handle-frame broker
                                client
                                (read-frame-from-client client))
               while (not (eq :closed state))))))))


;;;;;;
;;; websocket-broker

(def class* websocket-request (http-request)
  ())

(def class* websocket-response (primitive-http-response)
  ((broker :type websocket-broker)))

(def method read-request :around ((server server) client-stream/iolib client-stream/ssl)
  (bind ((request (call-next-method)))
    (when (websocket-request? request)
      (change-class request 'websocket-request))
    request))

(def method call-if-matches-request ((broker websocket-broker) (request websocket-request) thunk)
  ;; we leverage the context path matching machinery of broker-at-path
  (when (call-next-method broker request (lambda () t))
    (produce-response broker request)))

(def method produce-response ((broker websocket-broker) (request websocket-request))
  (bind ((path (path-of broker))
         (origin (header-value request "Origin"))
         (host (header-value request "Host"))
         (websocket-key (header-value request "Sec-WebSocket-Key"))
         (websocket-accept (base64:usb8-array-to-base64-string
                            (ironclad:digest-sequence
                             'ironclad:sha1
                             (ironclad:ascii-string-to-byte-array
                              (string+ websocket-key +websocket-magic-key+))))))
    (server.debug "PRODUCE-RESPONSE for ~A on path ~A, request ~A" broker path request)
    (make-instance 'websocket-response
                   :broker broker
                   :headers `(("Content-Type" . "application/octet-stream")
                              ("Status" . "101 Switching Protocols")
                              ("Sec-WebSocket-Accept" . ,websocket-accept)
                              ("Sec-WebSocket-Origin" . ,origin)
                              ("Sec-WebSocket-Location" . ,host)
                              ("Connection" . "Upgrade")
                              ("Upgrade" . "WebSocket")))))

(def method send-response ((response websocket-response))
  (assert (not (headers-are-sent-p response)) () "The headers of ~A have already been sent, this is a program error." response)
  (server.debug "SEND-RESPONSE for ~A " response)
  (setf (headers-are-sent-p response) #t)
  (setf (header-value response +header/status+) +http-switching-protocols+)
  (send-headers response)
  (bind ((stream (client-stream-of *request*))
         (broker (broker-of response)))
    (force-output stream)
    (let ((new-client (make-instance 'websocket-client
                             :input-stream stream
                             :output-stream stream)))
      (call-with-new-client-for-broker new-client
                                         broker
                                         #'(lambda ()
                                             (catch 'websocket-done
                                               (handler-bind ((error #'(lambda (e)
                                                                         (maybe-invoke-debugger e)
                                                                         (server.error "Error: ~a" e)
                                                                         (throw 'websocket-done nil))))
                                                 (read-handle-loop broker new-client))))))))

(defun websocket-request? (request)
  (and (search "upgrade" (header-value request +header/connection+) :test #'string-equal)
       (search "websocket" (header-value request +header/upgrade+) :test #'string-equal)))

(def (definer e) websocket-entry-point ((application path message &optional (message-type nil) (client nil) (other-clients nil) &key (priority 0)) &body body)
  "Creates a websocket-broker and a websocket-message-received method which specialises on that
  broker. The &body code is called when a websocket client connects to the given path and sends a text or binary
  message."
  (with-unique-names (entry-point)
    `(bind ((,entry-point (make-instance 'websocket-broker :path ,path :priority ,priority )))
       ,(unless body
                (error "You must define a websocket-entry-point with a body which does something with type and message."))
       (defmethod websocket-message-received ((broker (eql ,entry-point))
                                              ,(or client (gensym "client"))
                                              ,(or message-type (gensym "message-type"))
                                              ,message)
           (bind ((,(or other-clients (gensym "others")) (remove ,(or client (gensym "client")) (clients-of broker))))
             ,@body))
       (ensure-entry-point ,application ,entry-point)
       ,entry-point)))

