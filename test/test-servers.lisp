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

(def function startup-test-server/project-file-server (&key (maximum-worker-count 16) (log-level +dribble+) (wait #t))
  (flet ((syspath (path)
           (system-relative-pathname :hu.dwim.web-server.test path)))
    (with-main-logger-level log-level
      (startup-test-server-with-brokers (list
                                         (make-directory-serving-broker "data" (syspath "test/data")
                                                                        :render-directory-index #f
                                                                        :priority 0)
                                         (make-directory-serving-broker "data/auto-dir-index" (syspath "test/data/auto-dir-index")
                                                                        :render-directory-index #t
                                                                        :priority 1)
                                         (make-directory-serving-broker "data/no-dir-index" (syspath "test/data/no-dir-index")
                                                                        :render-directory-index #f
                                                                        :priority 1))

                                        :maximum-worker-count maximum-worker-count
                                        :wait wait))))

(def function startup-test-server/performance (&key (maximum-worker-count 4) (log-level +warn+))
  (with-main-logger-level log-level
    (startup-test-server-with-brokers (make-functional-broker
                                        (with-request-parameters (name)
                                          (make-functional-html-response ()
                                            (emit-html-document (:title "foo")
                                              <h3 ,(or name "The name query parameter is not specified!")>))))
                                      :maximum-worker-count maximum-worker-count)))
