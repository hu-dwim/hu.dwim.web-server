;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(eval-when (:compile-toplevel :load-toplevel)
  (def (function io) guess-encoding-for-http-response ()
    (or (awhen (and *response*
                    (external-format-of *response*))
          (encoding-name-of it))
        +default-encoding+))

  (def (function eio) content-type-for (mime-type &optional encoding)
    (unless encoding
      (setf encoding (guess-encoding-for-http-response)))
    ;; this is a somewhat ugly optimization: return constants for the most often used combinations
    (or (case encoding
          (:utf-8    (case mime-type
                       (+html-mime-type+       +utf-8-html-content-type+)
                       (+xhtml-mime-type+      +utf-8-xhtml-content-type+)
                       (+css-mime-type+        +utf-8-css-content-type+)
                       (+javascript-mime-type+ +utf-8-javascript-content-type+)
                       (t (switch (mime-type :test #'string=)
                            (+html-mime-type+       +utf-8-html-content-type+)
                            (+xhtml-mime-type+      +utf-8-xhtml-content-type+)
                            (+css-mime-type+        +utf-8-css-content-type+)
                            (+javascript-mime-type+ +utf-8-javascript-content-type+)))))
          (:us-ascii (case mime-type
                       (+html-mime-type+       +us-ascii-html-content-type+)
                       (+xhtml-mime-type+      +us-ascii-xhtml-content-type+)
                       (+css-mime-type+        +us-ascii-css-content-type+)
                       (+javascript-mime-type+ +us-ascii-javascript-content-type+)
                       (t (switch (mime-type :test #'string=)
                            (+html-mime-type+       +us-ascii-html-content-type+)
                            (+xhtml-mime-type+      +us-ascii-xhtml-content-type+)
                            (+css-mime-type+        +us-ascii-css-content-type+)
                            (+javascript-mime-type+ +us-ascii-javascript-content-type+)))))
          (:iso8859-1 (case mime-type
                        (+html-mime-type+       +iso-8859-1-html-content-type+)
                        (+xhtml-mime-type+      +iso-8859-1-xhtml-content-type+)
                        (+css-mime-type+        +iso-8859-1-css-content-type+)
                        (+javascript-mime-type+ +iso-8859-1-javascript-content-type+)
                        (t (switch (mime-type :test #'string=)
                             (+html-mime-type+       +iso-8859-1-html-content-type+)
                             (+xhtml-mime-type+      +iso-8859-1-xhtml-content-type+)
                             (+css-mime-type+        +iso-8859-1-css-content-type+)
                             (+javascript-mime-type+ +iso-8859-1-javascript-content-type+))))))
        (string+ mime-type "; charset=" (string-downcase encoding)))))

(def (constant e) +html-content-type+         (content-type-for +html-mime-type+         +default-encoding+))
(def (constant e) +xhtml-content-type+        (content-type-for +xhtml-mime-type+        +default-encoding+))
(def (constant e) +xml-content-type+          (content-type-for +xml-mime-type+          +default-encoding+))
(def (constant e) +javascript-content-type+   (content-type-for +javascript-mime-type+   +default-encoding+))

(def function emit-xhtml-prologue (encoding doctype &optional (stream *xml-stream*))
  (declare (ignore encoding))
  (macrolet ((emit (string)
               `(write-string ,(if (stringp string)
                                   (coerce string 'simple-base-string)
                                   string)
                              stream)))
    (if (eq doctype +xhtml-1.1-doctype+)
        (emit #.(format nil "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"))
        (progn
          (emit #.(format nil "<!DOCTYPE html PUBLIC "))
          (emit doctype)
          (emit #.(format nil ">~%"))))))

(def (macro e) emit-http-response ((&optional headers-as-plist cookie-list) &body body)
  "Emit a full http response and also bind html stream, so you are ready to output directly into the network stream."
  `(emit-into-xml-stream (client-stream-of *request*)
     (server.dribble "EMIT-HTTP-RESPONSE will now send the headers")
     (send-http-headers (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                      (collect `(cons ,name ,value))))
                        (list ,@cookie-list))
     (server.dribble "EMIT-HTTP-RESPONSE will now run the &body forms")
     ,@body))

(def (macro e) emit-http-response* ((&optional headers cookies) &body body)
  "Just like EMIT-HTML-RESPONSE, but HEADERS and COOKIES are simply evaluated as expressions."
  `(emit-into-xml-stream (client-stream-of *request*)
     (send-http-headers ,headers ,cookies)
     ,@body))

(def (macro e) emit-http-response/simple-html-document ((&key title status (cacheable #t) headers cookies) &body body)
  `(emit-http-response* ((append
                          ,@(when headers (list headers))
                          ,@(when status `((list (cons +header/status+ ,status))))
                          (unless ,cacheable
                            +disallow-response-caching-header-values+)
                          '((#.+header/content-type+ . #.+utf-8-html-content-type+)))
                         ,cookies)
     (emit-html-document (:content-type +html-content-type+ :title ,title)
       ,@body)))

(def special-variable *xhtml-body-environment*)

(def special-variable *xhtml-body-environment-wrappers* '(js-script-collapser/wrapper
                                                          js-onload-callbacks/wrapper))

(def with-macro* with-xhtml-body-environment (&key (wrappers *xhtml-body-environment-wrappers*))
  (labels ((call-in-environment (thunk wrappers)
             (if wrappers
                 (progn
                   (log.debug "Wrapping xhtml body with ~S" (first wrappers))
                   (funcall (first wrappers) (lambda ()
                                               (call-in-environment thunk (rest wrappers)))))
                 (funcall thunk))))
    (call-in-environment #'-with-macro/body- wrappers)))

(def function js-script-collapser/wrapper (thunk)
  (bind ((result nil)
         (script-body (with-output-to-sequence (*js-stream* :element-type (if *transform-quasi-quote-to-binary*
                                                                              '(unsigned-byte 8)
                                                                              'character)
                                                            :external-format (if *response*
                                                                                 (external-format-of *response*)
                                                                                 +default-encoding+))
                        (setf result (funcall thunk)))))
    (append
     (ensure-list result)
     (unless (zerop (length script-body))
       (list (hu.dwim.quasi-quote::as-delayed-emitting
               (write-sequence #.(format nil "<script type=\"text/javascript\">// <![CDATA[~%") *xml-stream*)
               (write-sequence script-body *xml-stream*)
               (write-sequence #.(format nil "~%// ]]></script>") *xml-stream*)))))))

(def special-variable *js-onload-callbacks*)

(def function js-onload-callbacks/wrapper (thunk)
  (bind ((*js-onload-callbacks* '()))
    (multiple-value-prog1
        (funcall thunk)
      (when *js-onload-callbacks*
        `js(foreach 'dojo.addOnLoad
                    ,(iter (initially (write-byte #.(char-code #\[) *js-stream*))
                           (for el :in (nreverse *js-onload-callbacks*))
                           (unless (first-time-p)
                             (write-byte #.(char-code #\,) *js-stream*))
                           (write-sequence #.(babel:string-to-octets "function () {" :encoding :utf-8) *js-stream*)
                           (write-sequence el *js-stream*)
                           (write-byte #.(char-code #\}) *js-stream*)
                           (finally
                            (write-byte #.(char-code #\]) *js-stream*)
                            (return +void+))))))))

(def (with-macro* e) emit-html-document (&key title
                                              content-type
                                              encoding
                                              head
                                              body-element-attributes
                                              (xhtml-doctype +xhtml-1.1-doctype+ xhtml-doctype-provided?)
                                              page-icon
                                              stylesheet-uris)
  (bind ((response *response*)
         (encoding (or encoding
                       (awhen (and response
                                   (external-format-of response))
                         (encoding-name-of it))
                       +default-encoding+))
         (content-type (or content-type
                           (when response
                             (header-value response +header/content-type+)))))
    (unless xhtml-doctype-provided?
      ;; TODO gracefully fall back to plain html if the request is coming from a crippled browser
      )
    (when xhtml-doctype
      (emit-xhtml-prologue encoding xhtml-doctype))
    <html (:xmlns     #.+xml-namespace-uri/xhtml+
           xmlns:dojo #.+xml-namespace-uri/dojo+)
     <head
      ,(when content-type
         <meta (:http-equiv #.+header/content-type+ :content ,content-type)>)
      ,(when page-icon
         <link (:rel "icon" :type "image/x-icon" :href ,page-icon)>)
      <title ,title>
      ,(foreach (lambda (stylesheet-uri)
                  <link (:rel "stylesheet" :type "text/css"
                              :href ,(if (stringp stylesheet-uri)
                                         (uri/percent-encoding/encode stylesheet-uri)
                                         (uri/print-to-string stylesheet-uri)))>)
                stylesheet-uris)
      ,@head>
     <body (,@body-element-attributes)
       ,@(with-xhtml-body-environment ()
           (list (-body-)))>>))

(def (macro e) with-request-parameters (args &body body)
  `(with-request-parameters* *request* ,args ,@body))

(def (macro e) with-request-parameters* (request args &body body)
  "Bind the parameters in REQUEST according to the REQUEST-LAMBDA-LIST and execute BODY.

REQUEST-LAMBDA-LIST is a list of the form:

 ( [ ( symbol string ) | symbol ]
   [ default-value [ supplied-symbol-name ]? ]? )

If the request contains a param (no distinction between GET and POST params is made) named STRING (which defaults to the symbol name of SYMBOL) the variable SYMBOL is bound to the associated value (which is always a string). If no parameter with that name was passed SYMBOL will be bound to DEFAULT-VALUE and the variable named SUPPLIED-SYMBOL-NAME will be bound to NIL."
  (once-only (request)
    (iter (for entry :in args)
          (setf entry (ensure-list entry))
          (unless (and (< 0 (length entry) 5)
                       (not (null (first entry))))
            (error "Invalid WITH-REQUEST-PARAMETERS entry ~S" entry))
          (for name-part = (ensure-list (first entry)))
          (unless (and (< 0 (length name-part) 3)
                       (not (null (first name-part))))
            (error "Invalid WITH-REQUEST-PARAMETERS entry ~S" entry))
          (for variable-name = (first name-part))
          (for parameter-name = (second name-part))
          (for default-value = (second entry))
          (for supplied-variable-name = (third entry))
          (unless parameter-name
            (setf parameter-name (etypecase variable-name
                                   (symbol (string-downcase variable-name))
                                   (string variable-name))))
          (collect `((:values ,variable-name ,@(when supplied-variable-name
                                                 (list supplied-variable-name)))
                     (request-parameter-value ,request ,parameter-name ,default-value))
            :into bindings)
          (finally (return `(bind ,bindings
                              ,@body))))))

;;;;;;
;;; Cookies

(def (function eio) make-cookie (name value &key comment domain max-age path secure)
  (rfc2109:make-cookie
   :name name
   :value (uri/percent-encoding/encode value)
   :comment comment
   :domain domain
   :max-age max-age
   :path (awhen path
           (uri/percent-encoding/encode it))
   :secure secure))
