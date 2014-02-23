;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.web-server.application.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.web-server.test
  :depends-on (:hu.dwim.logger
               :hu.dwim.web-server.application
               :hu.dwim.web-server.test)
  :components ((:module "test"
                :components ((:module "applications"
                              :components ((:file "authentication")
                                           (:file "dojo")
                                           (:file "echo")
                                           (:file "parameter")
                                           (:file "performance")
                                           (:file "session")))
                             (:file "test-applications" :depends-on ("applications"))))))
