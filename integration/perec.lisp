;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (class* e) application-with-perec-support (application)
  ((database nil)))

(def method produce-response :around ((application application-with-perec-support) request)
  (hu.dwim.rdbms:with-database (database-of application)
    (call-next-method)))

(def method call-in-application-environment :around ((application application-with-perec-support) session thunk)
  (bind ((database (database-of application)))
    (unless database
      (error "No database is specified for ~A" application))
    (hu.dwim.rdbms:with-readonly-transaction
      (hu.dwim.perec:with-new-compiled-query-cache
        (call-next-method)))))
