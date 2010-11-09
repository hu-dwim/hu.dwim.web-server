;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def condition* broker-recursion-limit-reached (request-processing-error)
  ((brokers :documentation "The stack of brokers we visited while reaching the limit"))
  (:report
   (lambda (error stream)
     (format stream "Broker recursion limit reached while calling brokers. Broker path: ~A~%" (brokers-of error)))))

(def function broker-recursion-limit-reached (request brokers)
  (error 'broker-recursion-limit-reached :request request :brokers brokers))
