;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; login-data

(def (class* ea) login-data ()
  ((extra-arguments '()))
  (:documentation "A login-data is an object that encapsulates information that should be used for authentication. It can be used for dispatching in later phases of the authentication. The EXTRA-ARGUMENTS slot can hold some &rest keyword arguments that is useful later on."))

(def (class* ea) login-data/identifier (login-data)
  ((identifier nil :type string)))

(def (class* ea) login-data/identifier-and-password (login-data/identifier)
  ((password nil :type string)))

(def print-object login-data/identifier-and-password
  (write-string "identifier: ")
  (write (identifier-of -self-)))

;;;;;;
;;; application-with-login-support

(def (class* e) application-with-login-support (application)
  ())

(def (class* ea) session-with-login-support (session)
  ((authenticate-return-value nil)))

(def method session-class list ((application application-with-login-support))
  'session-with-login-support)

(def method is-logged-in? ((session session-with-login-support))
  (not (null (authenticate-return-value-of session))))

;;;;;;
;;; Login

(def function login-current-session (login-data)
  (bind ((session (login *application* *session* login-data)))
    (check-type session session)
    (setf *session* session)))

(def method login ((application application-with-login-support) (session null) login-data)
  (app.debug "~S is making a new web session and calling itself with it" 'login)
  (setf session (make-new-session application))
  ;; recall ourselves with a valid session to dispatch to the other method(s)
  (login application session login-data) ; login signals if there's any error, including authentication failure
  (app.debug "Registering new web session ~A" session)
  (with-lock-held-on-application (application)
    (register-session application session))
  session)

(def method login ((application application-with-login-support) (session session-with-login-support) login-data)
  (app.debug "LOGIN called with login-data ~A" login-data)
  (if (is-logged-in? session)
      (restart-case
          (progn
            (app.dribble "LOGIN will signal 'ERROR/ALREADY-LOGGED-IN now")
            (error/already-logged-in login-data))
        (logout-and-continue ()
          :report (lambda (stream)
                    (format stream "~@<Call LOGOUT and retry calling LOGIN~@:>"))
          (logout application session)
          (setf session (login application *session* login-data))))
      (bind ((authenticate-return-value (authenticate application session login-data)))
        (app.debug "AUTHENTICATE returned ~S" authenticate-return-value)
        (unless authenticate-return-value
          (error/login-failed login-data "~S returned false" 'authenticate))
        (setf (authenticate-return-value-of session) authenticate-return-value)))
  session)

;;;;;;
;;; Logout

(def (function e) logout-current-session ()
  (logout *application* *session*)
  ;; set *session* to nil here so that the session cookie removal is decorated on the response. otherwise the next request to an entry point
  ;; would send up with a session id to an invalid session and trigger HANDLE-REQUEST-TO-INVALID-SESSION.
  (setf *session* nil)
  (setf *frame* nil))

(def method logout ((application application-with-login-support) (session session-with-login-support))
  (mark-session-invalid session)
  (setf (authenticate-return-value-of session) nil))
