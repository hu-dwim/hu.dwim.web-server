;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (constant e) +login-identifier-cookie-name+           "login-identifier")
(def (constant e) +login-entry-point-path+                 "login/")
(def (constant e) +session-timed-out-query-parameter-name+ "timed-out")
(def (constant e) +user-action-query-parameter-name+       "user-action")
(def (constant e) +continue-url-query-parameter-name+      "continue-url")

(def (function e) extract-login-data/identifier-and-password ()
  (with-request-parameters (identifier password)
    (string/trim-whitespace-and-maybe-nil-it identifier)
    (string/trim-whitespace-and-maybe-nil-it password)
    (make-instance 'login-data/identifier-and-password
                   :identifier identifier
                   :password password)))

;;; these are not with-macro's because their body needs several variables. functionally it's clearer what's going on...

(def (function e) call-with-entry-point-logic/login-with-identifier-and-password (response-factory &rest extra-login-arguments &key &allow-other-keys)
  (apply 'call-with-entry-point-logic/login response-factory 'extract-login-data/identifier-and-password
         extra-login-arguments))

(def (function e) call-with-entry-point-logic/login (response-factory login-data-extractor &rest extra-login-arguments &key
                                                                      &allow-other-keys)
  (with-request-parameters (continue-url
                            ((user-action? +user-action-query-parameter-name+) #f)
                            ((timed-out? +session-timed-out-query-parameter-name+) #f))
    (declare (ignore timed-out?))
    (string/trim-whitespace-and-maybe-nil-it continue-url)
    (with-entry-point-logic (:with-optional-session/frame/action-logic #t)
      (bind ((login-data (funcall login-data-extractor))
             (new-session? #f)
             (authentication-failure-reason nil)
             (authentication-happened? #f))
        (assert login-data)
        (app.debug "WITH-ENTRY-POINT-LOGIC/LOGIN, login-data is ~S, user-action? is ~S, continue-url is ~S" login-data user-action? continue-url)
        (progn
          (setf (extra-arguments-of login-data) extra-login-arguments)
          (if (or (null *session*)
                  (not (is-logged-in? *session*)))
              (block call-login
                (handler-bind ((error/authentication (lambda (error)
                                                       (setf authentication-failure-reason error)
                                                       (return-from call-login nil))))
                  (app.dribble "WITH-ENTRY-POINT-LOGIC/LOGIN will now call LOGIN")
                  (bind ((new-session (login *application* *session* login-data)))
                    (check-type new-session session)
                    (setf new-session? (not (eq *session* new-session)))
                    (setf *session* new-session)))
                (setf authentication-happened? #t))
              ;; TODO support re-authentication here when the model supports it
              (app.debug "WITH-ENTRY-POINT-LOGIC/LOGIN skipped calling LOGIN because *session* is already logged in")))
        (bind ((response (cond
                           ((and continue-url
                                 *session*
                                 (is-logged-in? *session*))
                            (bind ((target-uri (parse-uri continue-url)))
                              (decorate-uri-for-current-application target-uri)
                              (make-redirect-response target-uri)))
                           (t
                            (funcall response-factory
                                     :login-data login-data
                                     :user-action? user-action?
                                     :authentication-failure-reason authentication-failure-reason
                                     :authentication-happened? authentication-happened?)))))
          (app.dribble "WITH-ENTRY-POINT-LOGIC/LOGIN received the response ~A" response)
          ;; it's a wierd situation if there's no response at this point, but let's keep the flexibility...
          (when response
            ;; and we return with the decorated response
            (when new-session?
              (setf response (decorate-session-cookie *application* response))))
          response)))))
