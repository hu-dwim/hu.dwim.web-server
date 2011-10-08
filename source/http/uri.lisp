;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def special-variable *clone-request-uri/default-strip-query-parameters* nil)

(def (function e) clone-request-uri (&key (strip-query-parameters *clone-request-uri/default-strip-query-parameters*)
                                          append-to-path)
  (bind ((uri (clone-uri (uri-of *request*))))
    (if (eq strip-query-parameters :all)
        (uri/delete-all-query-parameters uri)
        (dolist (parameter-name (ensure-list strip-query-parameters))
          (uri/delete-query-parameters uri parameter-name)))
    (when append-to-path
      (etypecase append-to-path
        (string
         (setf (path-of uri) (string+ (path-of uri) append-to-path)))))
    uri))
