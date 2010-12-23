;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def function startup-test-server/request-echo-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-main-logger-level log-level
    (startup-test-server-with-handler (lambda ()
                                        (bind ((response (make-request-echo-response)))
                                          (unwind-protect
                                               (send-response response)
                                            (close-response response))))
                                      :maximum-worker-count maximum-worker-count)))

(def function startup-test-server/project-file-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-main-logger-level log-level
    (startup-test-server-with-brokers (make-directory-serving-broker "/wui/" (system-relative-pathname :hu.dwim.web-server.test ""))
                                      :maximum-worker-count maximum-worker-count)))

(def function startup-test-server/performance (&key (maximum-worker-count 4) (log-level +warn+))
  (with-main-logger-level log-level
    (startup-test-server-with-brokers (make-functional-broker
                                        (with-request-parameters (name)
                                          (make-functional-html-response ()
                                            (emit-html-document (:title "foo")
                                              <h3 ,(or name "The name query parameter is not specified!")>))))
                                      :maximum-worker-count maximum-worker-count)))
