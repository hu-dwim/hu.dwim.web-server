;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (function io) header-alist-value (alist header-name)
  ;; NOTE: header names are not case sensitive
  (assoc-value alist header-name :test #'equalp))

;; NOTE: it cannot be a simple (setf header-alist-value) function, because the place of the alist needs to be updated upon new insertion
(def macro %set-header-alist-value (alist header-name new-value)
  `(setf (assoc-value ,alist ,header-name :test #'equalp) ,new-value))
(defsetf header-alist-value %set-header-alist-value)

(def macro disallow-response-caching-in-header-alist (headers)
  (with-unique-names (header-name header-value)
    `(iter (for (,header-name . ,header-value) :in +disallow-response-caching-header-values+)
           (setf (header-alist-value ,headers ,header-name) ,header-value))))

(def macro enforce-response-caching-in-header-alist (headers)
  `(setf
    ;; the w3c spec requires a maximum age of 1 year
    ;; Firefox 3+ needs 'public' to cache this resource when received via SSL
    (header-alist-value ,headers +header/cache-control+) "public max-age=31536000"
    (header-alist-value ,headers +header/expires+) (local-time:to-rfc1123-timestring
                                                    (local-time:adjust-timestamp (local-time:now) (offset :year 1)))))

(def (function o) send-http-headers (headers cookies &key (stream (client-stream-of *request*)))
  (labels ((write-crlf (stream)
             (write-byte +carriage-return+ stream)
             (write-byte +linefeed+ stream))
           (write-header-line (name value)
             (http.dribble "Sending header ~S: ~S" name value)
             (write-sequence (string-to-us-ascii-octets name) stream)
             (write-sequence #.(string-to-us-ascii-octets ": ") stream)
             (write-sequence (string-to-iso-8859-1-octets value) stream)
             (write-crlf stream)
             (values)))
    (bind ((status (or (header-alist-value headers +header/status+)
                       +http-ok+))
           (date-header-seen? #f)
           (connection-header-seen? #f))
      (http.debug "Sending headers (Status: ~S)" status)
      (write-sequence #.(string-to-us-ascii-octets "HTTP/1.1 ") stream)
      (write-sequence (string-to-us-ascii-octets status) stream)
      (write-byte +space+ stream)
      (write-crlf stream)
      (dolist ((name . value) headers)
        (when (equalp name +header/date+)
          (setf date-header-seen? #t))
        (when (equalp name +header/connection+)
          (setf connection-header-seen? #t))
        (when value
          (write-header-line name value)))
      (unless date-header-seen?
        (write-header-line +header/date+ (local-time:to-rfc1123-timestring (local-time:now))))
      ;; TODO: connection keep-alive handling
      (unless connection-header-seen?
        (write-header-line +header/connection+ "Close"))
      (dolist (cookie cookies)
        (write-header-line "Set-Cookie"
                           (if (rfc2109:cookie-p cookie)
                               (rfc2109:cookie-string-from-cookie-struct cookie)
                               cookie)))
      (write-crlf stream)
      status)))

;;;;;;
;;; accept-header

(def (function o) parse-accept-language-header-value (header-value)
  (check-type header-value string)
  (bind ((*print-pretty* #f)
         (result (parse-accept-header-value header-value)))
    (labels ((convert-to-canonical-locale-name (key)
               (bind ((language nil)
                      (territory nil))
                 (iter (for char :in-vector key)
                       (for index :upfrom 0)
                       (when (char= char #\-)
                         (setf language (subseq key 0 index))
                         (setf territory (subseq key (1+ index)))
                         (return)))
                 (if territory
                     (with-output-to-string (*standard-output* nil :element-type 'base-char)
                       (write-string language)
                       (write-char #\_)
                       (write-string (string-upcase territory)))
                     key))))
      (iter (for entry :in result)
            (setf (car entry) (convert-to-canonical-locale-name (car entry)))))
    result))

(def (function o) parse-accept-header-value (header-value)
  (check-type header-value string)
  (http.dribble "Parsing Accept header ~S" header-value)
  (bind ((*print-pretty* #f)
         (index 0)
         (length (length header-value))
         (entries ())
         (key)
         (score))
    (declare (type array-index index))
    (labels ((fail ()
               (error "Failed to parse accept header value ~S" header-value))
             (make-string-buffer ()
               (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
             (next-char ()
               (when (< index length)
                 (aref header-value index)))
             (read-next-char ()
               (bind ((result (next-char)))
                 (incf index)
                 (if (and result
                          (member result '(#\Space #\Tab #\Newline #\Linefeed) :test #'char=))
                     (read-next-char)
                     result)))
             (parse-key ()
               (setf score nil)
               (setf key nil)
               (iter (for char = (read-next-char))
                     (case char
                       (#\; (parse-score))
                       (#\, (emit-entry))
                       ((nil) (emit-entry)
                              (emit-result))
                       (t
                        (unless key
                          (setf key (make-string-buffer)))
                        (vector-push-extend char key)))))
             (parse-score ()
               (unless (char= #\q (read-next-char))
                 (fail))
               (unless (char= #\= (read-next-char))
                 (fail))
               (setf score (make-string-buffer))
               (iter (for char = (read-next-char))
                     (if (and char
                              (or (alphanumericp char)
                                  (char= char #\.)))
                         (vector-push-extend char score)
                         (case char
                           (#\,
                            (emit-entry))
                           ((nil)
                            (emit-entry)
                            (emit-result))
                           (t
                            (fail))))))
             (emit-entry ()
               ;; (break "emitting ~S" (cons key score))
               (when key
                 (push (cons key
                             (if score
                                 (parse-number:parse-number score)
                                 1))
                       entries))
               (when (next-char)
                 (parse-key)))
             (emit-result ()
               (return-from parse-accept-header-value
                 (sort entries #'> :key #'cdr))))
      (parse-key))))
