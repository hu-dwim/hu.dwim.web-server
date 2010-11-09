;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def condition* too-many-sessions (request-processing-error)
  ((application nil))
  (:report
   (lambda (error stream)
     (format stream "Too many active session for application ~S, the server seems to be overloaded."
             (application-of error)))))

(def function too-many-sessions (application)
  (error 'too-many-sessions :request *request* :application application))

(def (condition* e) error/authentication (error)
  ())

(def condition* error/login-failed (simple-error
                                    error/authentication)
  ((login-data)))

(def function error/login-failed (login-data format-control &rest format-arguments)
  (error 'error/login-failed :format-control format-control :format-arguments format-arguments :login-data login-data))

(def condition* error/already-logged-in (error/login-failed)
  ())

(def function error/already-logged-in (login-data)
  (error 'error/already-logged-in :format-control "This session is already logged in" :login-data login-data))
