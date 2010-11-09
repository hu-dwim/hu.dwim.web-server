;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def special-variable *performance-application* (make-instance 'standard-application :path-prefix "/performance/"))

(def entry-point (*performance-application* :path "")
  (with-request-parameters (name)
    (make-functional-html-response ()
      (emit-html-document ()
        <h3 ,(or name "The name query parameter is not specified!")>))))
