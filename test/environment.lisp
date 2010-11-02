;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.test)

(def macro with-wui-logger-level (log-level &body body)
  `(with-logger-level (wui ,log-level)
    ,@body))

(def special-variable *muffle-compiler-warnings* t
  "Muffle or not the compiler warnings while the tests are running.")

(def macro with-test-compiler-environment (&body body)
  `(handler-bind ((style-warning
                   (lambda (c)
                     (when *muffle-compiler-warnings*
                       (muffle-warning c)))))
    ,@body))

(def suite* (test :in root-suite) (&key (log-level +warn+))
  (with-wui-logger-level log-level
    (with-test-compiler-environment
      (-run-child-tests-))))

(def definer test (name args &body body)
  `(def hu.dwim.stefil::test ,name ,args
    ;; rebind these, so that we can setf it freely in the tests...
    (bind ((*test-application* *test-application*)
           (*test-server* *test-server*))
      ,@body)))

(def special-variable *test-host* +any-host+ "The test server host.")

(def special-variable *test-port* 8080 "The test server port.")

(def special-variable *test-server* nil "The currently running test server.")

(def special-variable *test-application* nil "The currently running test application.")
