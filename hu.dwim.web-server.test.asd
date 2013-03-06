;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.web-server.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :package-name :hu.dwim.web-server.test
  :depends-on (:drakma
               :hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.web-server
               :hu.dwim.web-server+swank)
  :components ((:module "test"
                :components ((:file "environment" :depends-on ("package"))
                             (:file "package")
                             (:file "test-servers" :depends-on ("environment"))))))
