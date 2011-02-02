;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

#|

most importantly because / is not the same as %2F

I'd guess that the correct order is to split on slashes -first-, then decode.
foom I think you will be RFCly correct if you split on slashes, then split on ;, then decode.

|#

;; http://www.faqs.org/rfcs/rfc2396.html

(export '(uri-of host-of scheme-of path-of fragment-of))

(define-condition uri-parse-error (simple-parse-error)
  ())

(def function uri-parse-error (message &rest args)
  (error 'uri-parse-error
         :format-control message
         :format-arguments args))

(def (class* e) uri ()
  ((scheme
    nil
    :type (or null string))
   (host
    nil
    :type (or null string))
   (port
    nil
    :type (or null string))
   (path
    nil
    :type (or null string))
   (query
    nil
    :type (or null string))
   (query-parameters
    :unbound
    :documentation "An internal cache for PARSE-QUERY-PARAMETERS."
    :type list)
   (fragment nil)))

(def method make-load-form ((self uri) &optional env)
  (make-load-form-saving-slots self :environment env))

(def print-object (uri :identity nil)
  (write-uri -self- *standard-output* :escape #f))

(def (function ie) make-uri (&key scheme host port path query fragment)
  (make-instance 'uri :scheme scheme :host host :port port :path path
                 :query query :fragment fragment))

(def (function ie) clone-uri (uri &key (scheme nil scheme-provided?) (host nil host-provided?) (port nil port-provided?)
                                  (path nil path-provided?) (query nil query-provided?) (fragment nil fragment-provided?))
  (bind ((result #.`(make-instance 'uri
                                   ,@(iter (for name :in '(scheme host port path query fragment))
                                           (collect (intern (string name) :keyword))
                                           (collect `(if ,(symbolicate name '#:-provided?)
                                                         ,name
                                                         (,(symbolicate name '#:-of) uri)))))))
    (when (and (not query-provided?)
               (slot-boundp uri 'query-parameters))
      (setf (query-parameters-of result) (copy-alist (query-parameters-of uri))))
    result))

(def special-variable *clone-request-uri/default-strip-query-parameters* nil)

(def (function e) clone-request-uri (&key (strip-query-parameters *clone-request-uri/default-strip-query-parameters*)
                                          append-to-path)
  (bind ((uri (clone-uri (uri-of *request*))))
    (if (eq strip-query-parameters :all)
        (uri/delete-all-query-parameters uri)
        (dolist (parameter-name (ensure-list strip-query-parameters))
          (uri/delete-query-parameters uri parameter-name)))
    (when append-to-path
      (etypecase append-to-path
        (string
         (setf (path-of uri) (string+ (path-of uri) append-to-path)))))
    uri))

(def method query-parameters-of :before ((self uri))
  (unless (slot-boundp self 'query-parameters)
    (setf (query-parameters-of self) (awhen (query-of self)
                                       (parse-query-parameters it)))))

(def (function e) uri-query-parameter-value (uri name)
  (cdr (assoc name (query-parameters-of uri) :test #'string=)))

(def (function e) (setf uri-query-parameter-value) (value uri name)
  (if value
      (setf (assoc-value (query-parameters-of uri) name :test #'string=) value)
      (removef (query-parameters-of uri) name :test #'string= :key #'car))
  value)

(def (function e) uri/delete-query-parameters (uri &rest names)
  (setf (query-parameters-of uri)
        (delete-if [member (car !1) names :test #'string=]
                   (query-parameters-of uri)))
  uri)

(def (function e) copy-uri-query-parameters (from to &rest parameter-names)
  (dolist (name parameter-names)
    (setf (uri-query-parameter-value to name)
          (uri-query-parameter-value from name))))

(def function add-query-parameter-to-uri (uri name value)
  (nconcf (query-parameters-of uri) (list (cons name value)))
  uri)

(def (function e) uri/delete-all-query-parameters (uri)
  (setf (query-parameters-of uri) '())
  uri)

(def (function e) append-path-to-uri (uri path-to-append)
  (setf (path-of uri) (string+ (path-of uri) path-to-append))
  uri)

(def (function e) prefix-uri-path (uri path-to-prefix)
  (setf (path-of uri) (string+ path-to-prefix (path-of uri)))
  uri)

(def (function o) write-uri-sans-query (uri stream &key (escape #t))
  "Write URI to STREAM, only write scheme, host and path."
  (bind ((scheme (scheme-of uri))
         (host (host-of uri))
         (port (port-of uri))
         (path (path-of uri)))
    (flet ((out (string)
             (funcall (if escape
                          #'write-as-uri
                          #'write-string)
                      string stream)))
      (when scheme
        (out scheme)
        (write-string "://" stream))
      (when host
        ;; don't escape host
        (etypecase host
          (iolib:ipv6-address
           (write-char #\[ stream)
           (write-string (iolib:address-to-string host) stream)
           (write-char #\] stream))
          (iolib:ipv4-address
           (write-string (iolib:address-to-string host) stream))
          (string
           (write-string host stream))))
      (when port
        (write-string ":" stream)
        (princ port stream))
      (when path
        (out path)))))

#+nil
(def (function o) write-relative-uri (uri stream &optional (escape t))
  "Write URI to STREAM, only write components starting with path."
  (bind ((path (path-of uri)))
    (flet ((out (string)
             (funcall (if escape
                          #'write-as-uri
                          #'write-string)
                      string stream)))
      (when path
        (out path))
      (write-query-parameters (query-parameters-of uri) stream :escape escape)
      (awhen (fragment-of uri)
        (write-char #\# stream)
        (out it)))))

(def (function o) write-query-parameters (parameters stream &key (escape t))
  (labels ((out (string)
             (funcall (if escape
                          #'write-as-uri
                          #'write-string)
                      string stream)
             (values))
           (write-query-part (name value)
             (if (consp value)
                 (iter (for el :in value)
                       (unless (first-time-p)
                         (write-char #\& stream))
                       (out name)
                       (write-char #\= stream)
                       (write-query-value el))
                 (progn
                   (out name)
                   (write-char #\= stream)
                   (write-query-value value))))
           (write-query-value (value)
             (out (typecase value
                    (integer (integer-to-string value))
                    (number (princ-to-string value))
                    (null "")
                    (t (string value))))))
    (iter (for (name . value) :in parameters)
          (write-char (if (first-time-p) #\? #\&) stream)
          (write-query-part name value))))

(def (function o) write-uri (uri stream &key (escape t) (extra-parameters '()))
  (write-uri-sans-query uri stream :escape escape)
  (labels ((out (string)
             (funcall (if escape
                          #'write-as-uri
                          #'write-string)
                      string stream)))
    (bind ((parameters (query-parameters-of uri)))
      (when extra-parameters
        (setf parameters (append extra-parameters parameters)))
      (write-query-parameters parameters stream :escape escape))
    (awhen (fragment-of uri)
      (write-char #\# stream)
      (out it))))

(def function ensure-uri-string (uri)
  (if (stringp uri)
      (escape-as-uri uri)
      (print-uri-to-string uri)))

(def (function e) print-uri-to-string (uri &key (escape #t) (extra-parameters '()))
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (string)
      (write-uri uri string :escape escape :extra-parameters extra-parameters))))

#+nil
(def (function e) print-relative-uri-to-string (uri &optional (escape t))
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (string)
      (write-relative-uri uri string escape))))

(def function print-uri-to-string-sans-query (uri &key (escape #t))
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (string)
      (write-uri-sans-query uri string :escape escape))))

(def (constant :test 'equalp) +uri-escaping-ok-table+
  (bind ((result (make-array 256
                             :element-type 'boolean
                             :initial-element nil)))
    ;; The list of characters which don't need to be escaped when writing URIs.
    ;; This list is inherently a heuristic, because different uri components may have
    ;; different escaping needs, but it should work fine for http.
    (loop
       :for ok-char across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_~/"
       :do (setf (aref result (char-code ok-char)) t))
    (coerce result '(simple-array boolean (256)))))

(def (function e) escape-as-uri (string)
  "Escapes all non alphanumeric characters in STRING following the URI convention. Returns a fresh string."
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (escaped nil :element-type 'base-char)
      (write-as-uri string escaped))))

(def (function o) write-as-uri (string stream)
  (declare (type vector string)
           (type stream stream))
  (loop
     :for char-code :of-type (unsigned-byte 8) :across (the (vector (unsigned-byte 8))
                                                         (string-to-utf-8-octets string))
     :do (if (aref #.+uri-escaping-ok-table+ char-code)
             (write-char (code-char char-code) stream)
             (format stream "%~2,'0X" char-code))))

(def (function io) unescape-as-uri (string)
  (%unescape-as-uri (coerce string 'simple-base-string)))

(def (function o) %unescape-as-uri (input)
  "URI unescape based on http://www.ietf.org/rfc/rfc2396.txt"
  (declare (type simple-base-string input))
  (let ((input-length (length input)))
    (when (zerop input-length)
      (return-from %unescape-as-uri ""))
    (bind ((seen-escaped? #f)
           (seen-escaped-non-ascii? #f)
           (input-index 0)
           (output (make-array input-length :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (declare (type fixnum input-length input-index))
      (labels ((read-next-char (must-exists-p)
                 (when (>= input-index input-length)
                   (if must-exists-p
                       (uri-parse-error "Unexpected end of input on ~S" input)
                       (return-from %unescape-as-uri (if seen-escaped?
                                                         (if seen-escaped-non-ascii?
                                                             (utf-8-octets-to-string output)
                                                             (us-ascii-octets-to-string output))
                                                         input))))
                 (prog1
                     (aref input input-index)
                   (incf input-index)))
               (write-next-byte (byte)
                 (declare (type (unsigned-byte 8) byte))
                 (when (> byte 127)
                   (setf seen-escaped-non-ascii? #t))
                 (vector-push-extend byte output)
                 (values))
               (char-to-int (char)
                 (let ((result (digit-char-p char 16)))
                   (unless result
                     (uri-parse-error "Expecting a digit and found ~S in ~S at around position ~S" char input input-index))
                   result))
               (parse ()
                 (let ((next-char (read-next-char nil)))
                   (case next-char
                     (#\% (char%))
                     (#\+ (char+))
                     (t (write-next-byte (char-code next-char))))
                   (parse)))
               (char% ()
                 (setf seen-escaped? #t)
                 (write-next-byte (+ (ash (char-to-int (read-next-char t)) 4)
                                     (char-to-int (read-next-char t))))
                 (values))
               (char+ ()
                 (setf seen-escaped? #t)
                 (write-next-byte +space+)))
        (parse)))))

(def (function eio) parse-uri (uri)
  (%parse-uri (coerce uri 'simple-base-string)))

(def (function o) %parse-uri (uri)
  (declare (type simple-base-string uri))
  ;; can't use :sharedp, because we expect the returned pieces to be simple-base-string's and :sharedp would return displaced arrays
  (bind ((pieces (nth-value 1 (cl-ppcre:scan-to-strings "^(([^:/?#]+):)?(//([^:/?#]*)(:([0-9]+)?)?)?([^?#]*)(\\?([^#]*))?(#(.*))?"
                                                        uri :sharedp #f))))
    (flet ((process (index)
             (bind ((piece (aref pieces index)))
               (values (if (and piece
                                (not (zerop (length piece))))
                           (unescape-as-uri piece)
                           nil)))))
      (declare (inline process)
               (dynamic-extent #'process))
      ;; call unescape-as-uri on each piece separately, so some of them may remain simple-base-string even if other pieces contain unicode
      ;; FIXME path needs to be split before unescape, to avoid coalescing %2F into /
      (make-uri :scheme   (process 1)
                :host     (process 3)
                :port     (process 5)
                :path     (process 6)
                :query    (aref pieces 8) ; query needs to be parsed before unescaping, see PARSE-QUERY-PARAMETERS
                :fragment (process 10)))))

(def macro record-query-parameter (param params)
  (declare (type cons param))
  (once-only (param)
    `(bind ((entry (assoc (car ,param) ,params :test #'string=)))
       (if entry
           (progn
             (unless (consp (cdr entry))
               (setf (cdr entry) (list (cdr entry))))
             (nconcf (cdr entry) (list (cdr ,param))))
           (push ,param ,params))
       ,params)))

(def (function o) parse-query-parameters (param-string &key initial-parameters (sideffect-initial-parameters #f))
  "Parse PARAM-STRING into an alist. The value part is a list when the given parameter was found multiple times."
  (declare (type simple-base-string param-string))
  (flet ((grab-param (start separator-position end)
           (declare (type array-index start end)
                    (type (or null array-index) separator-position))
           (bind ((key-start start)
                  (key-end (or separator-position end))
                  (key (make-displaced-array param-string key-start key-end))
                  (value-start (if separator-position
                                   (1+ separator-position)
                                   end))
                  (value-end end)
                  (value (if (zerop (- value-end value-start))
                             ""
                             (make-displaced-array param-string value-start value-end)))
                  (unescaped-key (unescape-as-uri key))
                  (unescaped-value (unescape-as-uri value)))
             (http.dribble "Grabbed parameter ~S with value ~S" unescaped-key unescaped-value)
             (cons unescaped-key unescaped-value))))
    (when (and param-string
               (< 0 (length param-string)))
      (iter
        (with start = 0)
        (with separator-position = nil)
        (with result = (if sideffect-initial-parameters
                           initial-parameters
                           (copy-alist initial-parameters)))
        (for char :in-vector param-string)
        (for index :upfrom 0)
        (switch (char :test #'char=)
          (#\& ;; end of the current param
           (setf result (record-query-parameter (grab-param start separator-position index) result))
           (setf start (1+ index))
           (setf separator-position nil))
          (#\= ;; end of name
           (setf separator-position index)))
        (finally
         (return (nreverse (record-query-parameter (grab-param start separator-position (1+ index)) result))))))))
