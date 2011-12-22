;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def macro with-main-logger-level (log-level &body body)
  `(with-logger-level (hu.dwim.web-server::log ,log-level)
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
  (with-main-logger-level log-level
    (with-test-compiler-environment
      (-run-child-tests-))))

(def definer test (name args &body body)
  `(def hu.dwim.stefil::test ,name ,args
    ;; rebind these, so that we can setf it freely in the tests...
    (bind ((*test-application* *test-application*)
           (*test-server* *test-server*))
      ,@body)))

(def special-variable *test-host* +any-host+
  "The test server host.")

(def special-variable *test-port* 8080
  "The test server port.")

(def special-variable *test-server* nil
  "The currently running test server.")

(def special-variable *test-application* nil
  "The currently running test application.")

(def special-variable *running-test-servers* (list)
  "All the servers that have been started by the test code and hasn't been shut down yet.")

(def function startup-test-server (server &key maximum-worker-count)
  (when maximum-worker-count
    (setf (maximum-worker-count-of server) maximum-worker-count))
  (finishes
    (is (not (is-server-running? server)))
    (write-string (test-server-info-string server) *debug-io*)
    (startup-server server)
    (unless (zerop (maximum-worker-count-of server))
      (is (is-server-running? server))
      (pushnew server *running-test-servers*)
      (setf *test-server* server)))
  server)

(def function shutdown-test-server (&optional (server *test-server*))
  (finishes
    (is (not (null server)))
    (dolist (listen-entry (listen-entries-of server))
      (is (not (null (socket-of listen-entry)))))
    (shutdown-server server)
    (dolist (listen-entry (listen-entries-of server))
      (is (null (socket-of listen-entry))))
    (setf *running-test-servers* (delete server *running-test-servers*))
    (when (eq server *test-server*)
      (setf *test-server* nil))
    (values)))

(def function startup-test-server-with-handler (handler &rest args &key
                                                        (wait #t)
                                                        (host *test-host*)
                                                        (port *test-port*)
                                                        (server-type 'server)
                                                        (maximum-worker-count 16) ; lower to 0 to start the server in the REPL thread
                                                        &allow-other-keys)
  (remove-from-plistf args :server-type)
  (bind ((server (apply #'make-instance server-type :host host :port port :maximum-worker-count maximum-worker-count args)))
    (setf (handler-of server) handler)
    (if wait
        (startup-test-server-and-wait server)
        (startup-test-server server))))

(def function startup-test-server-with-brokers (brokers &rest args &key
                                                        (wait #t)
                                                        (host *test-host*)
                                                        (port *test-port*)
                                                        (server-type 'broker-based-server)
                                                        (maximum-worker-count 16) ; lower to 0 to start the server in the REPL thread
                                                        &allow-other-keys)
  (when *test-server*
    (cerror "Start anyway" "*TEST-SERVER* is not NIL which means that there's a test server still running. You can use (SHUTDOWN-TEST-SERVER) to shut it down. See also *RUNNING-TEST-SERVERS*."))
  (remove-from-plistf args :server-type)
  (bind ((server (apply #'make-instance server-type :host host :port port :maximum-worker-count maximum-worker-count args)))
    (setf (brokers-of server) (ensure-list brokers))
    (if wait
        (startup-test-server-and-wait server)
        (startup-test-server server))))

(def function test-server-info-string (server)
  (bind ((listen-entry (first (listen-entries-of server)))
         (host (address-to-string (host-of listen-entry)))
         (port (port-of listen-entry))
         (uri (hu.dwim.uri:make-uri :scheme "http" :host host :port port))
         (uri-string (hu.dwim.uri:print-uri-to-string uri)))
    (format nil "Server ~A is now running. You can use (~S) to stop it, or select an appropriate restart if you see this inside the debugger. See also ~S and ~S.~%~
                 You may stress the server with one of the command lines below, but keep in mind that logging and generally not loading the code in ~S severly hurts performance.~%~
                 siege --concurrent=100 --time=10S ~A (add -b for full throttle benchmarking)~%~
                 httperf --rate 1000 --num-conn 10000 --port ~A --server ~A --uri /foo/bar/~%~
                 ab -n 100000 -c10 http://127.0.0.1:8080/performance~%~
                 "
            server
            'shutdown-test-server
            '*test-server*
            '*running-test-servers*
            '*load-as-production?*
            uri-string
            port
            host)))

(def function startup-test-server-and-wait (server)
  (unwind-protect
       (progn
         (startup-test-server server)
         (break (test-server-info-string server)))
    (shutdown-test-server server))
  server)
