;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def special-variable *clone-request-uri/default-strip-query-parameters* nil)

;; TODO rename to unwanted-parameters or somesuch
(def (function e) clone-request-uri (&key (strip-query-parameters *clone-request-uri/default-strip-query-parameters*)
                                          append-to-path)
  (bind ((uri (hu.dwim.uri:clone-uri (uri-of *request*))))
    (if (eq strip-query-parameters :all)
        (hu.dwim.uri:delete-all-query-parameters uri)
        (dolist (parameter-name (ensure-list strip-query-parameters))
          (hu.dwim.uri:delete-query-parameters uri parameter-name)))
    (when append-to-path
      (etypecase append-to-path
        (string
         (setf (path-of uri) (string+ (hu.dwim.uri:path-of uri) append-to-path)))))
    uri))
