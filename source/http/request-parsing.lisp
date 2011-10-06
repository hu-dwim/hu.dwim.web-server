;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (function io) parse-http-version (version-string)
  (declare (type string version-string))
  (flet ((fail-unless (condition)
           (unless condition
             (illegal-http-request/error "Illegal http version string ~S" version-string))))
    (declare (inline fail-unless))
    (fail-unless (starts-with-subseq "HTTP/" version-string))
    (bind ((dot-position (position #\. version-string :start 5)))
      (fail-unless dot-position)
      (bind ((major-version (parse-integer version-string :start 5 :end dot-position))
             ((:values minor-version end-position) (parse-integer version-string :start (1+ dot-position))))
        (fail-unless (= (length version-string) end-position))
        (values major-version minor-version)))))

(def (function o) read-http-request/head (client-fd client-stream/ssl ssl-stream-handle &key (length-limit *length-limit/http-request-head*))
  (check-type length-limit (integer 512))
  (bind ((buffer-length length-limit)
         (buffer/lisp (make-array buffer-length :element-type '(unsigned-byte 8)))
         (marker #.(coerce-to-simple-ub8-vector
                    (list +carriage-return+ +linefeed+ +carriage-return+ +linefeed+)))
         (marker-length (length marker))
         (position 0)
         (marker-position 0)
         ;; KLUDGE deadline stuff should be hadled by a call/cc based multiplexer dropping dry connections
         (start-time (get-monotonic-time))
         ;; FIXME random inline constant
         (deadline (+ start-time 15)))
    (cffi-sys:with-pointer-to-vector-data (buffer buffer/lisp)
      (iter
        (while (< marker-position marker-length))
        (when (> (get-monotonic-time) deadline)
          (error 'iolib.syscalls:etimedout :handle client-fd :message "READ-HTTP-REQUEST/HEAD timed out"))
        (when (>= position buffer-length)
          (request-length-limit-reached 'read-http-request/head buffer-length position))
        (for bytes-read = (bind ((bytes-to-read (- marker-length marker-position)) ; we can read as many bytes as the number of bytes left in the marker
                                 (read-buffer-pointer (cffi-sys:inc-pointer buffer position)))
                            (if ssl-stream-handle
                                (cl+ssl::ensure-ssl-funcall client-stream/ssl
                                                            ssl-stream-handle
                                                            #'cl+ssl::ssl-read
                                                            ssl-stream-handle
                                                            read-buffer-pointer
                                                            bytes-to-read)
                                (handler-case
                                    (iolib.syscalls:read client-fd read-buffer-pointer bytes-to-read)
                                  (iolib.syscalls:ewouldblock ()
                                    ;; TODO implement the call/cc based multiplexer and get rid of this...
                                    (sleep 0.1)
                                    (next-iteration))))))
        (assert (integerp bytes-read))
        (iter
          (repeat bytes-read)
          (if (= (cffi:mem-ref buffer :char position)
                 (aref marker marker-position))
              (incf marker-position)
              (setf marker-position 0))
          (incf position))))
    (shrink-vector buffer/lisp position)))

(def (function o) parse-http-request/head (buffer https?)
  (handler-bind ((uri-parse-error (lambda (error)
                                    (illegal-http-request/error (princ-to-string error)))))
    (bind (((:values http-method raw-uri version-string position) (parse-http-request-line buffer)))
      (http.dribble "In PARSE-HTTP-REQUEST/HEAD; http-method is ~S, raw-uri is ~S, version is ~S" http-method raw-uri version-string)
      (bind (((:values major-version minor-version) (parse-http-version version-string))
             (raw-uri-length (length raw-uri))
             (headers (aprog1
                          (parse-http-request-headers buffer position)
                        (http.dribble "Request headers are ~S" it))))
        (bind ((host (or (assoc-value headers "Host" :test #'equal)
                         (illegal-http-request/error "No \"Host\" header in the request")))
               (host-length (length host))
               (scheme (if https? "https" "http"))
               (scheme-length (length scheme))
               (uri-string (bind ((position 0)
                                  (result (make-string (+ scheme-length #.(length "://") host-length raw-uri-length)
                                                       :element-type 'base-char)))
                             (replace result scheme)
                             (replace result #.(coerce "://" 'simple-base-string) :start1 (incf position scheme-length))
                             (replace result host :start1 (incf position #.(length "://")))
                             (replace result raw-uri :start1 (incf position host-length))
                             result))
               (uri (%parse-uri uri-string)))
          (http.dribble "Request query parameters from the uri: ~S" (query-parameters-of uri))
          ;; extend the parameters with the possible stuff in the request body
          ;; making sure duplicate entries are recorded in a list keeping the original order.
          (values http-method raw-uri version-string major-version minor-version
                  uri headers))))))

(def (function o) take-iso-8859-1-substring-from-ub8-vector (buffer start end)
  ;; the base-char optimization may not necessarily be worth it...
  (bind ((result (if (find-if [>= !1 128] buffer :start start :end end)
                     (make-string (- end start) :element-type 'character)
                     (make-string (- end start) :element-type 'base-char))))
    (iter (for source-index :first start :then (1+ source-index))
          (for destination-index :upfrom 0)
          (while (< source-index end))
          ;; usage of CODE-CHAR: iso-8859-1 coincides with the first 256 code points of unicode
          (setf (aref result destination-index) (code-char (aref buffer source-index))))
    result))

(def (function io) is-alphanumeric-char-code? (byte)
  (or (<= #.(char-code #\a) byte #.(char-code #\z))
      (<= #.(char-code #\A) byte #.(char-code #\Z))
      (<= #.(char-code #\0) byte #.(char-code #\9))))

(declaim (ftype (function (simple-ub8-vector) (values string string (or null string) array-index)) parse-http-request-line))
(def (function o) parse-http-request-line (buffer)
  "A simple state machine which reads chars from CLIENT-FD until it gets a CR-LF sequence. Signals an error upon EOF."
  (declare (type simple-ub8-vector buffer))
  (bind ((buffer/length (length buffer))
         (position 0)
         (http-method nil)
         (uri nil)
         (uri/start-position nil)
         (version nil)
         (version/start-position nil))
    (declare (type array-index position buffer/length))
    (labels ((is-line-end? (byte)
               (or (eql byte #.+carriage-return+)
                   (eql byte #.+linefeed+)))
             (next-byte ()
               (when (>= position buffer/length)
                 (illegal-http-request/error "~S ran out of available bytes in the buffer" 'parse-http-request-line))
               (prog1
                   (aref buffer position)
                 (incf position)))
             (fail-unless-linefeed ()
               (unless (eql +linefeed+ (next-byte))
                 (illegal-http-request/error "Expecting a line-feed after a carriage-return character")))
             (http-method ()
               (bind ((next-byte (next-byte)))
                 (cond
                   ((eql next-byte #.+space+)
                    (take-http-method)
                    (uri))
                   ((is-line-end? next-byte)
                    (illegal-http-request/error "Premature end of the first line of the HTTP request while parsing the HTTP method part"))
                   ((not (is-alphanumeric-char-code? next-byte))
                    (illegal-http-request/error "Illegal character ~S while parsing the HTTP method part" next-byte))
                   (t
                    (http-method)))))
             (take-http-method ()
               (setf http-method (take-iso-8859-1-substring-from-ub8-vector buffer 0 (1- position)))
               (setf uri/start-position position))
             (uri ()
               (bind ((next-byte (next-byte)))
                 (case next-byte
                   (#.+space+
                    (take-uri)
                    (version))
                   (#.+carriage-return+
                    (take-uri)
                    (fail-unless-linefeed))
                   (t
                    (uri)))))
             (take-uri ()
               (setf uri (take-iso-8859-1-substring-from-ub8-vector buffer uri/start-position (1- position)))
               (setf version/start-position position))
             (version ()
               (bind ((next-byte (next-byte)))
                 (case next-byte
                   (#.+carriage-return+
                    (take-version)
                    (fail-unless-linefeed))
                   (t
                    (version)))))
             (take-version ()
               (setf version (take-iso-8859-1-substring-from-ub8-vector buffer version/start-position (1- position)))))
      (http-method))
    (unless (member http-method +valid-http-methods+ :test #'string=)
      (illegal-http-request/error "Illegal HTTP method ~S" http-method))
    ;; TODO check uri and version for valid characters? or later while parsing them?
    (values http-method uri version position)))

(def (function o) parse-http-request-headers (buffer position)
  (check-type buffer simple-ub8-vector)
  (check-type position array-index)
  (bind ((buffer/length (length buffer))
         (headers '()))
    (labels ((next-byte ()
               (when (>= position buffer/length)
                 (illegal-http-request/error "~S ran out of available bytes in the buffer" 'parse-http-request-headers))
               (prog1
                   (aref buffer position)
                 (incf position)))
             (fail-unless-linefeed ()
               (unless (eql +linefeed+ (next-byte))
                 (illegal-http-request/error "Expecting a line-feed after a carriage-return character")))
             (header-line ()
               (bind ((name nil)
                      (name/start-position position)
                      (value nil)
                      (value/start-position nil))
                 (labels ((name/start ()
                            (case (next-byte)
                              (#.(char-code #\:)
                               (illegal-http-request/error "Zero length name in a HTTP header line"))
                              (#.+carriage-return+
                               (fail-unless-linefeed))
                              (t
                               (name))))
                          (name ()
                            (case (next-byte)
                              (#.(char-code #\:)
                               (setf name (take-iso-8859-1-substring-from-ub8-vector buffer name/start-position (1- position)))
                               (name/skip-whitespace))
                              (#.(list +carriage-return+ +linefeed+)
                               (illegal-http-request/error "Premature end of line while parsing a HTTP header name"))
                              (t
                               (name))))
                          (name/skip-whitespace ()
                            (case (next-byte)
                              ((#.(char-code #\Space)
                                #.(char-code #\Tab))
                               (name/skip-whitespace))
                              (t
                               (setf value/start-position (1- position))
                               (value))))
                          (value ()
                            (case (next-byte)
                              (#.+carriage-return+
                               (setf value (take-iso-8859-1-substring-from-ub8-vector buffer value/start-position (1- position)))
                               (fail-unless-linefeed))
                              (t
                               (value)))))
                   (name/start)
                   (when name
                     (when (zerop (length name))
                       (illegal-http-request/error "Zero length name in a HTTP header line"))
                     (push (cons name value) headers)
                     (header-line))))))
      (header-line))
    (nreverse headers)))

(def (function o) make-rfc2388-callback-factory (form-data-accumulator file-accumulator)
  (named-lambda rfc2388-callback-factory (mime-part)
    (http.dribble "Processing mime part ~S." mime-part)
    (bind ((content-disposition-header (rfc2388-binary:get-header mime-part "Content-Disposition"))
           (content-disposition (rfc2388-binary:header-value content-disposition-header))
           (name (rfc2388-binary:get-header-attribute content-disposition-header "name"))
           (filename (rfc2388-binary:get-header-attribute content-disposition-header "filename")))
      (http.dribble "Got a mime part. Disposition: ~S; Name: ~S; Filename: ~S" content-disposition name filename)
      (http.dribble "Mime Part:---~%~A---" (with-output-to-string (dump) (rfc2388-binary:print-mime-part mime-part dump)))
      (when (and filename
                 (equal filename ""))
        (setf filename nil))
      (cond
        ((or (string-equal "file" content-disposition)
             (not (null filename)))
         (bind (((:values file file-name) (open-temporary-file :file-name-prefix "hdws-file-upload-")))
           (flet ((close-and-delete-temporary-file ()
                    (close file)
                    (delete-file file-name)))
             (unwind-protect-case ()
                 (progn
                   (setf (rfc2388-binary:content mime-part) file-name)
                   (http.dribble "Sending mime part data to file ~S (~S)" file-name (rfc2388-binary:content mime-part))
                   (bind ((counter 0)
                          (buffer (make-array 8196 :element-type '(unsigned-byte 8)))
                          (buffer-length (length buffer))
                          (buffer-index 0))
                     (declare (type array-index buffer-length buffer-index counter))
                     (values (named-lambda mime/byte-handler (byte)
                               (declare (type (unsigned-byte 8) byte))
                               ;;(http.dribble "File byte ~4,'0D: ~D~:[~; (~C)~]" counter byte (<= 32 byte 127) (code-char byte))
                               (setf (aref buffer buffer-index) byte)
                               (incf counter)
                               (incf buffer-index)
                               (when (>= buffer-index buffer-length)
                                 (write-sequence buffer file)
                                 (setf buffer-index 0)))
                             (named-lambda mime/finish-callback ()
                               (http.dribble "Done with file ~S" (rfc2388-binary:content mime-part))
                               (unless (zerop buffer-index)
                                 (write-sequence buffer file :end buffer-index))
                               (http.dribble "Closing ~S" (rfc2388-binary:content mime-part))
                               (close file)
                               (http.dribble "Closed, calling file-accumulator ~S" file-accumulator)
                               (funcall file-accumulator name mime-part)
                               (values))
                             #'close-and-delete-temporary-file)))
                 (:abort (close-and-delete-temporary-file))))))
        ((string-equal "form-data" content-disposition)
         (http.dribble "Grabbing mime-part data as string.")
         (setf (rfc2388-binary:content mime-part) (make-adjustable-vector 10 :element-type '(unsigned-byte 8)))
         (bind ((counter 0))
           (declare (type array-index counter))
           (values (lambda (byte)
                     (declare (type (unsigned-byte 8) byte))
                     (http.dribble "Form-data byte ~4,'0D: ~D~:[~; (~C)~]." counter byte (<= 32 byte 127) (code-char byte))
                     (incf counter)
                     (vector-push-extend byte (rfc2388-binary:content mime-part)))
                   (lambda ()
                     ;; TODO fixed utf-8?
                     (bind ((content (utf-8-octets-to-string (rfc2388-binary:content mime-part))))
                       (http.dribble "Done with form-data ~S: ~S" name content)
                       (funcall form-data-accumulator name content)
                       (values))))))
        (t
         (illegal-http-request/error "Don't know how to handle the mime-part ~S (content-disposition: ~S)" mime-part content-disposition-header))))))

(def (function o) parse-http-request/body (stream raw-content-length raw-content-type initial-parameter-alist)
  (when (and raw-content-length
             raw-content-type)
    (with-thread-name " / PARSE-HTTP-REQUEST/BODY"
      (bind ((content-length (parse-integer raw-content-length :junk-allowed t)))
        (when (and content-length
                   (> content-length 0))
          (when (> content-length *length-limit/http-request-body*)
            (request-length-limit-reached 'parse-http-request/body *length-limit/http-request-body* content-length))
          (bind (((:values content-type attributes) (rfc2388-binary:parse-header-value raw-content-type))
                 ;; later on we may sideffect this alist, so copy
                 (final-parameter-alist (copy-alist initial-parameter-alist)))
            (switch (content-type :test #'string=)
              ("application/x-www-form-urlencoded"
               ;; TODO dos prevention, lower limit here than *length-limit/http-request-body* or separate for files
               (bind ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
                 (read-sequence buffer stream)
                 (bind ((buffer-as-string
                         (aif (cdr (assoc "charset" attributes :test #'string=))
                              ;; TODO check the standard, disable unescape in parse-query-parameters if needed...
                              (eswitch (it :test #'string-equal)
                                ("utf-8"      (utf-8-octets-to-string buffer))
                                ("ascii"      (us-ascii-octets-to-string buffer))
                                ("iso-8859-1" (iso-8859-1-octets-to-string buffer)))
                              (us-ascii-octets-to-string buffer))))
                   (http.dribble "Parsing application/x-www-form-urlencoded body. Attributes: ~S, value: ~S" attributes buffer-as-string)
                   ;; TODO buffer-as-string should never be non-ascii... read up on the standard, do something about that coerce...
                   (setf buffer-as-string (coerce buffer-as-string 'simple-base-string))
                   (return-from parse-http-request/body (parse-query-parameters buffer-as-string
                                                                                :initial-parameters final-parameter-alist
                                                                                :sideffect-initial-parameters #t)))))
              ("multipart/form-data"
               (http.dribble "Parsing multipart/form-data body. Attributes: ~S." attributes)
               (bind ((boundary (cdr (assoc "boundary" attributes :test #'string=)))
                      ;; no need to copy the alist, because we only push to its head
                      (final-parameter-alist initial-parameter-alist))
                 ;; TODO DOS prevention: add support for rfc2388-binary to limit parsing length if the ContentLength header is fake, pass in *length-limit/http-request-body*
                 (rfc2388-binary:read-mime stream boundary
                                           (make-rfc2388-callback-factory
                                            (lambda (name value)
                                              (record-query-parameter (cons name value) final-parameter-alist))
                                            (lambda (name file-mime-part)
                                              (record-query-parameter (cons name file-mime-part) final-parameter-alist))))
                 (return-from parse-http-request/body final-parameter-alist)))
              (t (illegal-http-request/error "Don't know how to handle content type ~S" content-type))))))))
  (http.debug "Skipped parsing request body, raw Content-Type is [~S], raw Content-Length is [~S]" raw-content-type raw-content-length)
  initial-parameter-alist)
