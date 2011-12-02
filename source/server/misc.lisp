;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def function trace-request-handling ()
  (trace broker/default-handler
         handle-request
         call-with-new-matching-uri-path-element
         call-if-matches-request
         produce-response
         query-brokers-for-response
         iterate-brokers-for-response
         produce-response/directory-serving
         handle-cgi-request
         ))

(def function make-default-broker-list (&key (include-application-support #t))
  "Returns a list of brokers that are needed in most situations."
  (bind ((priority 100))
    (append
     (list (make-instance 'js-directory-serving-broker
                          :path "hdws/js"
                          :root-directory (system-relative-pathname :hu.dwim.web-server "source/js/")
                          :priority priority)
           (make-instance 'js-i18n-broker :priority priority))
     (when include-application-support
       (list (make-directory-serving-broker "/static/hdws/" (system-relative-pathname :hu.dwim.web-server "www/") :priority priority))))))

;;;;;;
;;; Server status

(def (function e) make-server-status-response ()
  (disallow-response-caching
   (make-byte-vector-response ((+header/content-type+ +utf-8-plain-text-content-type+))
     (write-server-status t))))

(def (function e) write-server-status (stream)
  (bind ((*print-pretty* #f)
         (start (local-time:now)))
    (format stream "Server:                                     ~A~%" *server*)
    (format stream "Current time:                               ~A~%" (local-time:format-timestring nil start))
    (format stream "Startup time:                               ~A~%" (local-time:format-timestring nil (started-at-of *server*)))
    (format stream "Uptime:                                     ~A days~%" (coerce (/ (local-time::timestamp-difference start (started-at-of *server*)) (* 24 60 60)) 'float))
    (format stream "Number of workers:                          ~A~%" (length (workers-of *server*)))
    (format stream "Number of workers occupied:                 ~A~%" (occupied-worker-count-of *server*))
    (format stream "Number of requests processed:               ~A~%" (processed-request-counter-of *server*))
    (format stream "Number of gracefully aborted requests:      ~A~%" (gracefully-aborted-request-count-of *server*))
    (format stream "Number of failed requests:                  ~A~%" (failed-request-count-of *server*))
    (format stream "Number of client connection resets:         ~A~%" (client-connection-reset-count-of *server*))
    (format stream "Number of live web sessions:                ~A~%" (iter (for broker :in (brokers-of *server*))
                                                                            ;; FIXME forward references for application stuff
                                                                            (when (typep broker 'application)
                                                                              (summing (hash-table-count (session-id->session-of broker))))))
    (terpri)
    (awhen (and (boundp '*application*)
                (symbol-value '*application*))
      (format stream "Application:                                ~A~%" it)
      (format stream "Number of requests to valid sessions:       ~A~%" (requests-to-sessions-count-of it)) ; FIXME forward reference
      (format stream "Sessions last purged at:                    ~,2F seconds since boot~%" (coerce (sessions-last-purged-at-of it) 'float)) ; FIXME forward reference
      (terpri))
    (format stream "Heap usage:                                 ~,2F MBytes~%" (/ (sb-kernel::dynamic-usage) 1024 1024))
    (format stream "Number of threads running:                  ~A~%" (length (sb-thread::list-all-threads)))
    ;;(format stream "Maximum heap size: ~,2F MB~%" (/ (sb-kernel::dynamic-) 1024d0 1024d0))
    (format stream "Elapsed time rendering this response:       ~,4F seconds~%" (local-time:timestamp-difference (local-time:now) start))))

;;;;;;
;;; http-user-agent

(def (load-time-constant e) +chrome-version-scanner+    (cl-ppcre:create-scanner "Chrome/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +opera-version-scanner+     (cl-ppcre:create-scanner "Opera/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +konqueror-version-scanner+ (cl-ppcre:create-scanner "Konqueror/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +safari-version-scanner+    (cl-ppcre:create-scanner "Safari/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +msie-version-scanner+      (cl-ppcre:create-scanner "MSIE ([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +mozilla-version-scanner+   (cl-ppcre:create-scanner "Mozilla/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +drakma-version-scanner+    (cl-ppcre:create-scanner "Drakma/([0-9]{1,}\.[0-9]{0,})"))

(def (namespace :test 'equal) http-user-agent)

(def class* http-user-agent ()
  ((raw-http-header-value :type (or null string))
   (kind :type keyword)
   (version :type number)
   (supported :type boolean :accessor supported?)))

(def print-object http-user-agent
  (print-http-user-agent -self- t))

(def function print-http-user-agent (self stream)
  (format stream "~A : ~A (~A)"
          (kind-of self) (version-of self)
          (if (supported? self)
              "supported"
              "unsupported")))

(def function identify-http-user-agent (request)
  (bind ((header-value (header-value request +header/user-agent+)))
    ;; the locks inside the namespace accessors are enough here
    (or (find-http-user-agent header-value :otherwise #f)
        (setf (find-http-user-agent header-value)
              (parse-http-user-agent header-value)))))

(def function parse-http-user-agent (header-value)
  (flet ((try (version-scanner kind minimum-required-version)
           (bind (((:values success? version-string) (cl-ppcre:scan-to-strings version-scanner header-value)))
             (when success?
               (bind ((version (or (ignore-errors
                                     ;; KLUDGE version can be 1.2.3 also
                                     (parse-number:parse-number (first-elt version-string)))
                                   0)))
                 (make-instance 'http-user-agent
                                :kind kind
                                :version version
                                :supported (>= version minimum-required-version)
                                :raw-http-header-value header-value))))))
    (aprog1
        (or (try +chrome-version-scanner+    :chrome    3)
            (try +opera-version-scanner+     :opera     9.6)
            (try +konqueror-version-scanner+ :konqueror 4.2)
            (try +safari-version-scanner+    :safari    4)
            (try +msie-version-scanner+      :msie      7) ;; TODO rename
            (try +mozilla-version-scanner+   :mozilla   5)
            (try +drakma-version-scanner+    :drakma    0)
            (make-instance 'http-user-agent
                           :raw-http-header-value header-value
                           :kind :generic
                           :version 0
                           :supported #f))
      (http.info "Determined user agent is ~A" it))))
