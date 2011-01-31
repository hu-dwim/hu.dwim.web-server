;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.web-server.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.presentation
               :hu.dwim.web-server.test)
  :components ((:module "documentation"
                :components ((:file "package")
                             (:file "web-server" :depends-on ("package"))))))
