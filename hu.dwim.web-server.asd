;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.web-server
  :class hu.dwim.system
  :description "An iolib based HTTP server."
  :long-description "Provides error handling, response compression, static file serving, quasi quoted JavaScript and quasi quoted XML serving."
  :depends-on (:babel
               :babel-streams
               :bordeaux-threads
               :cffi
               :cl-fad ; TODO replace with iolib
               :hu.dwim.common
               :hu.dwim.computed-class+hu.dwim.defclass-star
               :hu.dwim.def.namespace
               :hu.dwim.def+cl-l10n
               :hu.dwim.def+contextl ; TODO no need for contextl here, factor out remaining dependencies
               :hu.dwim.def+hu.dwim.delico
               :hu.dwim.logger
               :hu.dwim.quasi-quote.xml+hu.dwim.quasi-quote.js
               :hu.dwim.syntax-sugar
               :hu.dwim.util.error-handling
               :hu.dwim.util.temporary-files
               :hu.dwim.util.zlib
               :iolib
               :iolib.pathnames
               :iolib.sockets
               :iolib.os
               :local-time
               :parse-number
               :rfc2109
               :rfc2388-binary
               ;; TODO: remove direct swank dependency
               :swank)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "configuration" :depends-on ("package"))
                             (:file "logger" :depends-on ("package"))
                             (:module "util"
                              :depends-on ("logger" "configuration")
                              :components ((:file "l10n" :depends-on ("util"))
                                           (:file "timer" :depends-on ("util"))
                                           (:file "util")
                                           (:file "timestring-parsing")))
                             (:module "http"
                              :depends-on ("logger" "util")
                              :components ((:file "headers" :depends-on ("conditions" "util" "variables"))
                                           (:file "conditions")
                                           (:file "production")
                                           (:file "request-parsing" :depends-on ("headers" "uri" "variables"))
                                           (:file "uri" :depends-on ("conditions" "variables"))
                                           (:file "util" :depends-on ("conditions" "variables"))
                                           (:file "variables")))
                             (:module "server"
                              :depends-on ("http")
                              :components ((:file "api" :depends-on ("conditions"))
                                           (:file "brokers" :depends-on ("server"))
                                           (:file "cgi" :depends-on ("brokers" "file-serving" "request-response"))
                                           (:file "conditions")
                                           (:file "error-handling" :depends-on ("api" "variables"))
                                           (:file "file-serving" :depends-on ("variables"))
                                           (:file "js-serving" :depends-on ("js-util" "file-serving"))
                                           (:file "js-i18n" :depends-on ("js-serving"))
                                           (:file "js-util" :depends-on ("variables"))
                                           (:file "misc" :depends-on ("request-response" "variables"))
                                           (:file "request-response" :depends-on ("api" "variables"))
                                           (:file "server" :depends-on ("variables" "error-handling"))
                                           (:file "variables" :depends-on ("api"))))))))

(defmethod perform :after ((op develop-op) (system (eql (find-system :hu.dwim.web-server))))
  (let ((*package* (find-package :hu.dwim.web-server)))
    (eval
     (read-from-string
      "(progn
         (setf (log-level 'log) +debug+)
         (setf *debug-on-error* t))")))
  (warn "Set hu.dwim.web-server log level to +debug+ and enabled server-side debugging"))
