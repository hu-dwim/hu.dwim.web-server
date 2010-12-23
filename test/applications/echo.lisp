;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def special-variable *echo-application* (make-instance 'standard-application :path-prefix "/echo/"))

(def entry-point (*echo-application* :path "")
  (make-request-echo-response))
