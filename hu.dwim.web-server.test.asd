;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.web-server.test
  :class hu.dwim.test-system
  :package-name :hu.dwim.web-server.test
  :depends-on (:drakma
               :hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.web-server
               :hu.dwim.web-server+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "environment" :depends-on ("package"))
                             (:file "http" :depends-on ("environment"))))))
