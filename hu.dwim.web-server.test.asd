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
               :hu.dwim.computed-class+hu.dwim.logger ; cc compiles with logger optionally. in dev builds we must make sure the logger is also loaded in with it, because the fasl's depend on it.
               :hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.web-server
               :hu.dwim.web-server+swank)
  :components ((:module "test"
                :components ((:file "environment" :depends-on ("package"))
                             (:file "package")
                             (:file "test-servers" :depends-on ("environment"))))))
