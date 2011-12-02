;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.web-server.application+hu.dwim.perec
  :class hu.dwim.system
  :depends-on (:hu.dwim.web-server.application
               :hu.dwim.perec)
  :components ((:module "integration"
                :components ((:file "perec")))))
