;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Specials available while processing a HTTP request

(def (special-variable e :documentation "The HTTP REQUEST currently being processed.")
  *request*)

(def (special-variable e :documentation "The HTTP RESPONSE for the HTTP REQUEST currently being processed.")
  *response*)

(def (special-variable e :documentation "The remote host which sent the currently processed HTTP REQUEST.")
  *request-remote-address*)

(def (special-variable e :documentation "The value of (iolib.sockets:address-to-string *request-remote-address*)")
  *request-remote-address/string*)

(def (special-variable :documentation "A unique identifier is assigned to each incoming request to help debugging. This variable holds that id while processing the request.")
  *request-id*)

(def (special-variable e) *disable-response-compression* #f
  "TRUE means that HTTP response will not be compressed, FALSE otherwise.")

(def (special-variable e) *request-content-length-limit* #.(* 5 1024 1024)
  "While uploading a file the size of the request may not go higher than this or WUI will signal an error.
See also the REQUEST-CONTENT-LENGTH-LIMIT slot of BASIC-BACKEND.")

;;;;;;
;;; Some DOS related limits

(def constant +maximum-http-request-header-line-count+ 128)

(def constant +maximum-http-request-header-line-length+ (* 4 1024))

;;;;;;
;;; l10n

(def constant +accept-language-cache-purge-size+ 1000
  "The maximum size of the cache of Accept-Language header over which the hashtable is cleared.")

(def constant +maximum-accept-language-value-length+ 100
  "The maximum size of the Accept-Language header value that is accepted.")

(macrolet ((x (&body entries)
             `(progn
                ,@(iter (for (name value) :on entries :by #'cddr)
                        (collect `(def (constant e) ,name ,value))
                        ;; we also define lower-case constants, because the quasi-quote readers are case sensitive
                        (collect `(def (constant e) ,(intern (string-downcase name)) ,value))))))
  (x
   +header/accept+              "Accept"
   +header/accept-charset+      "Accept-Charset"
   +header/accept-encoding+     "Accept-Encoding"
   +header/accept-language+     "Accept-Language"
   +header/accept-ranges+       "Accept-Ranges"
   +header/authorization+       "Authorization"
   +header/proxy-authorization+ "Proxy-Authorization"
   +header/connection+          "Connection"
   +header/date+                "Date"
   +header/host+                "Host"
   +header/referer+             "Referer"
   +header/refresh+             "Refresh"
   +header/if-modified-since+   "If-Modified-Since"
   +header/user-agent+          "User-Agent"
   +header/forwarded-for+       "X-Forwarded-For"
   ;; response
   +header/status+              "Status"
   +header/age+                 "Age"
   +header/allow+               "Allow"
   +header/cache-control+       "Cache-Control"
   +header/expires+             "Expires"
   +header/content-encoding+    "Content-Encoding"
   +header/content-language+    "Content-Language"
   +header/content-length+      "Content-Length"
   +header/content-location+    "Content-Location"
   +header/content-disposition+ "Content-Disposition"
   +header/content-md5+         "Content-MD5"
   +header/content-range+       "Content-Range"
   +header/content-type+        "Content-Type"
   +header/last-modified+       "Last-Modified"
   +header/location+            "Location"
   +header/server+              "Server"
   ))

(macrolet ((x (&body entries)
             `(progn
                ,@(iter (for (name value) :on entries :by #'cddr)
                        (collect `(def constant ,name (coerce ,value 'simple-base-string)))
                        (collect `(export ',name))))))
  (x
   +content-encoding/deflate+   "deflate"
   +content-encoding/gzip+      "gzip"
   ))

(macrolet ((x (&rest pairs)
             `(progn
                ,@(iter (for (name value) :on pairs :by #'cddr)
                        (collect `(progn
                                    (def (constant e) ,name (coerce ,value 'simple-base-string))
                                    ;; we also define lower-case constants, because the quasi-quote readers are case sensitive
                                    (def (constant e) ,(intern (string-downcase name)) (coerce ,value 'simple-base-string))))))))
  ;; constants for optimization
  (x
   +utf-8-html-content-type+            "text/html; charset=utf-8"
   +utf-8-xhtml-content-type+           "application/xhtml+xml; charset=utf-8"
   +utf-8-css-content-type+             "text/css; charset=utf-8"
   +utf-8-xml-content-type+             "text/xml; charset=utf-8"
   +utf-8-plain-text-content-type+      "text/plain; charset=utf-8"
   +utf-8-javascript-content-type+      "text/javascript; charset=utf-8"

   +us-ascii-html-content-type+         "text/html; charset=us-ascii"
   +us-ascii-xhtml-content-type+        "application/xhtml+xml; charset=us-ascii"
   +us-ascii-css-content-type+          "text/css; charset=us-ascii"
   +us-ascii-xml-content-type+          "text/xml; charset=us-ascii"
   +us-ascii-plain-text-content-type+   "text/plain; charset=us-ascii"
   +us-ascii-javascript-content-type+   "text/javascript; charset=us-ascii"

   +iso-8859-1-html-content-type+       "text/html; charset=iso-8859-1"
   +iso-8859-1-xhtml-content-type+      "application/xhtml+xml; charset=iso-8859-1"
   +iso-8859-1-css-content-type+        "text/css; charset=iso-8859-1"
   +iso-8859-1-xml-content-type+        "text/xml; charset=iso-8859-1"
   +iso-8859-1-plain-text-content-type+ "text/plain; charset=iso-8859-1"
   +iso-8859-1-javascript-content-type+ "text/javascript; charset=iso-8859-1"

   +html-mime-type+       "text/html"
   +xhtml-mime-type+      "application/xhtml+xml"
   +xml-mime-type+        "text/xml"
   +svg-xml-mime-type+    "image/svg+xml"
   +css-mime-type+        "text/css"
   +text-mime-type+       "text/plain"
   +csv-mime-type+        "text/csv"
   +pdf-mime-type+        "application/pdf"
   +odt-mime-type+        "application/vnd.oasis.opendocument.text"
   +ods-mime-type+        "application/vnd.oasis.opendocument.spreadsheet"
   +javascript-mime-type+ "text/javascript"
   +plain-text-mime-type+ "text/plain"
   +octet-stream-mime-type+ "application/octet-stream"
   +atom-feed-mime-type+  "application/atom+xml"

   +xhtml-1.0-strict-doctype+       "\"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\""
   +xhtml-1.0-transitional-doctype+ "\"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/transitional.dtd\""
   +xhtml-1.0-frameset-doctype+     "\"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\""
   +xhtml-1.1-doctype+              "\"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\""
   ))

(def (function io) mime-type= (a b)
  (declare (type simple-base-string a b))
  (or (eq a b)
      (string= a b)))

;;;;;;
;;; HTTP

(def constant +disallow-response-caching-header-values+
  (list (cons +header/expires+ "Wed, 01 Mar 2000 00:00:00 GMT")
        (cons +header/cache-control+ "no-cache no-store must-revalidate")))

(def symbol-macro +default-external-format+ (load-time-value (ensure-external-format +default-encoding+)))

(def (constant e) +xml-namespace-uri/xhtml+ "http://www.w3.org/1999/xhtml")
(def (constant e) +xml-namespace-uri/dojo+  "http://www.dojotoolkit.org/2004/dojoml")
(def (constant e) +xml-namespace-uri/atom+  "http://www.w3.org/2005/Atom")

(def (constant e) +form-encoding/multipart-form-data+  "multipart/form-data")
(def (constant e) +form-encoding/url-encoded+  "application/x-www-form-urlencoded")

(def constant +space+           #.(char-code #\Space))
(def constant +tab+             #.(char-code #\Tab))
(def constant +colon+           #.(char-code #\:))
(def constant +linefeed+        10)
(def constant +carriage-return+ 13)

(macrolet ((x (&body defs)
             `(progn
                ,@(iter (for (name value reason-phrase) :on defs :by #'cdddr)
                        #+nil ;; not used anywhere
                        (collect `(def constant ,(format-symbol *package*
                                                                "~A-CODE+"
                                                                (subseq (string name) 0
                                                                        (1- (length (string name)))))
                                      ,value))
                        (collect `(def (constant e) ,name
                                      ,(format nil "~A ~A" value reason-phrase)))))))
  (x
    +http-continue+                        100 "Continue"
    +http-switching-protocols+             101 "Switching Protocols"
    +http-ok+                              200 "OK"
    +http-created+                         201 "Created"
    +http-accepted+                        202 "Accepted"
    +http-non-authoritative-information+   203 "Non-Authoritative Information"
    +http-no-content+                      204 "No Content"
    +http-reset-content+                   205 "Reset Content"
    +http-partial-content+                 206 "Partial Content"
    +http-multi-status+                    207 "Multi-Status"
    +http-multiple-choices+                300 "Multiple Choices"
    +http-moved-permanently+               301 "Moved Permanently"
    +http-moved-temporarily+               302 "Moved Temporarily"
    +http-see-other+                       303 "See Other"
    +http-not-modified+                    304 "Not Modified"
    +http-use-proxy+                       305 "Use Proxy"
    +http-temporary-redirect+              307 "Temporary Redirect"
    +http-bad-request+                     400 "Bad Request"
    +http-authorization-required+          401 "Authorization Required"
    +http-payment-required+                402  "Payment Required"
    +http-forbidden+                       403 "Forbidden"
    +http-not-found+                       404 "Not Found"
    +http-method-not-allowed+              405 "Method Not Allowed"
    +http-not-acceptable+                  406 "Not Acceptable"
    +http-proxy-authentication-required+   407 "Proxy Authentication Required"
    +http-request-time-out+                408 "Request Time-out"
    +http-conflict+                        409 "Conflict"
    +http-gone+                            410 "Gone"
    +http-length-required+                 411 "Length Required"
    +http-precondition-failed+             412 "Precondition Failed"
    +http-request-entity-too-large+        413 "Request Entity Too Large"
    +http-request-uri-too-large+           414 "Request-URI Too Large"
    +http-unsupported-media-type+          415 "Unsupported Media Type"
    +http-requested-range-not-satisfiable+ 416 "Requested range not satisfiable"
    +http-expectation-failed+              417 "Expectation Failed"
    +http-failed-dependency+               424 "Failed Dependency"
    +http-internal-server-error+           500 "Internal Server Error"
    +http-not-implemented+                 501 "Not Implemented"
    +http-bad-gateway+                     502 "Bad Gateway"
    +http-service-unavailable+             503 "Service Unavailable"
    +http-gateway-time-out+                504 "Gateway Time-out"
    +http-version-not-supported+           505 "Version not supported"))
