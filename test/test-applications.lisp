;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def function startup-test-server/with-test-applications (&key (maximum-worker-count 16) (log-level +debug+))
  (with-main-logger-level log-level
    (startup-test-server-with-brokers (list (make-redirect-broker "/performance" "/performance/")
                                            *performance-application*
                                            (make-redirect-broker "/echo" "/echo/")
                                            *echo-application*
                                            (make-redirect-broker "/parameter" "/parameter/")
                                            *parameter-application*
                                            (make-redirect-broker "/session" "/session/")
                                            *session-application*
                                            (make-redirect-broker "/authentication" "/authentication/")
                                            *authentication-application*)
                                      :maximum-worker-count maximum-worker-count)))
