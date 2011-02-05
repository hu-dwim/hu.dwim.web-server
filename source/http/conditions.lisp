;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

#|

TODO delme?

(def (condition* e) no-handler-for-request (error)
  ((raw-uri nil)
   (request (when (boundp '*request*)
              *request*)))
  (:report (lambda (error stream)
             (format stream "No handler for query: ~S~%" (or (raw-uri-of error)
                                                             (awhen (request-of error)
                                                               (raw-uri-of it))
                                                             "#<unknown>")))))

(def function no-handler-for-request (&optional (request *request*))
  (error 'no-handler-for-request
         :request request))
|#

(def condition* request-processing-error (error)
  ((request (when (boundp '*request*)
              *request*))))

(def (condition* e) request-length-limit-reached (request-processing-error)
  ((assumed-content-length nil)
   (content-length-limit nil)
   (operation nil))
  (:report (lambda (error stream)
             (format stream "The size of the request is larger than what's allowed by the server policy (~A >= ~A, in operation ~S); see ~S and/or ~S."
                     (assumed-content-length-of error)
                     (content-length-limit-of error)
                     (operation-of error)
                     '*length-limit/http-request-head*
                     '*length-limit/http-request-body*))))

(def function request-length-limit-reached (operation content-length-limit &optional assumed-content-length)
  (error 'request-length-limit-reached
         :assumed-content-length assumed-content-length
         :content-length-limit content-length-limit
         :operation operation))

(def (condition e) access-denied-error (request-processing-error)
  ())

(def (function e) access-denied-error ()
  (error 'access-denied-error))

(def condition illegal-http-request/error (simple-error request-processing-error)
  ())

(def function illegal-http-request/error (&optional (format-control nil format-control?) &rest format-arguments)
  (error 'illegal-http-request/error
         :format-control (if format-control?
                             (string+ "Illegal HTTP request: " format-control)
                             "Illegal HTTP request")
         :format-arguments format-arguments))
