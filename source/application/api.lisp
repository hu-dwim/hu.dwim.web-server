;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (generic e) handle-toplevel-error/application/emit-response (application error ajax-aware?)
  (:documentation "Just like HANDLE-TOPLEVEL-ERROR/EMIT-RESPONSE but only called in the context of an application."))

(def (generic e) make-new-session (application))
(def (generic e) make-new-frame (application session))

(def (generic e) register-session (application session))
(def (generic e) register-frame (application session frame))

(def (generic e) purge-sessions (application)
  (:documentation "Purge the web sessions of APPLICATION. Make sure you don't hold locks to any resources while calling this method! Especially dangerous to call this method from code invoked by hu.dwim.web-server, because then you most probably have something locked already, e.g. the current SESSION object."))
(def (generic e) purge-frames (application session))

(def (generic e) delete-session (application session))

(def (generic e) session-class (application)
  (:documentation "Returns a list of the session mixin classes.

Custom implementations should look something like this:
\(def method session-class list ((app your-application))
  'your-session-mixin)")
  (:method-combination list))

(def (generic e) make-frame-root-component-using-application (application session frame component)
  (:documentation "Should create a new FRAME/WIDGET component for APPLICATION with the provided COMPONENT as the main content."))

(def (generic e) produce-response/application (application request)
  (:documentation "Just like PRODUCE-RESPONSE, but only the inner part which is wrapped by the application related bookkeping."))

(def (generic e) call-in-application-environment (application session thunk)
  (:documentation "Everything related to an application goes through this method, so it can be used to set up wrappers like WITH-TRANSACTION. The SESSION argument may or may not be a valid session.")
  (:method (application session thunk)
    (app.dribble "CALL-IN-APPLICATION-ENVIRONMENT is calling the thunk")
    (funcall thunk))
  (:method :around (application session thunk)
    (when (boundp 'call-in-application-environment/guard)
      (cerror "ignore" "~S has been called recursively" 'call-in-application-environment))
    (bind ((call-in-application-environment/guard #t)
           (*context-of-error* application))
      (declare (special call-in-application-environment/guard))
      (call-next-method))))

(def (generic e) call-in-post-action-environment (application session frame thunk)
  (:documentation "This call wraps entry points and rendering, but does not wrap actions. The SESSION argument may or may not be a valid session.")
  (:method (application session frame thunk)
    (app.dribble "CALL-IN-POST-ACTION-ENVIRONMENT is calling the thunk")
    (funcall thunk)))

(def (generic e) call-in-entry-point-environment (application session thunk)
  (:method (application session thunk)
    (bind ((*inside-user-code* #t))
      (funcall thunk))))

(def (generic e) call-action (application session frame action)
  (:method (application session frame (action function))
    (funcall action))
  (:method :around (application session frame action)
    (bind ((*inside-user-code* #t))
      (call-next-method))))

(def type session-invalidity-reason ()
  `(member :nonexistent :timed-out :invalidated))

(def type frame-invalidity-reason ()
  `(member :nonexistent :timed-out :invalidated :out-of-sync))

(def type action-invalidity-reason ()
  `(member :nonexistent :timed-out :invalidated))

(def (generic e) handle-request-to-invalid-session (application session invalidity-reason)
  (:method :before (application session invalidity-reason)
    (check-type invalidity-reason session-invalidity-reason)
    (check-type session (or null session))
    (app.dribble "HANDLE-REQUEST-TO-INVALID-SESSION invoked. Application ~A, session ~A, invalidity-reason ~S, *ajax-aware-request* ~S" application session invalidity-reason *ajax-aware-request*)))

(def (generic e) handle-request-to-invalid-frame (application session frame invalidity-reason)
  (:method :before (application session frame invalidity-reason)
    (check-type invalidity-reason frame-invalidity-reason)
    (check-type frame (or null frame))
    (app.dribble "HANDLE-REQUEST-TO-INVALID-FRAME invoked. Application ~A, session ~A, frame ~A, invalidity-reason ~S, *ajax-aware-request* ~S" application session frame invalidity-reason *ajax-aware-request*)))

(def (generic e) handle-request-to-invalid-action (application session frame action invalidity-reason)
  (:method :before (application session frame action invalidity-reason)
    (check-type invalidity-reason action-invalidity-reason)
    (check-type action (or null action))
    (app.dribble "HANDLE-REQUEST-TO-INVALID-ACTION invoked. Application ~A, session ~A, frame ~A, action ~A, invalidity-reason ~S, *ajax-aware-request* ~S" application session frame action invalidity-reason *ajax-aware-request*)))

(def generic entry-point-equals-for-redefinition (a b)
  (:method (a b)
    #f)
  (:method :around (a b)
           (if (eql (class-of a) (class-of b))
               (call-next-method)
               #f)))

;;;;;;
;;; authentication (needs an APPLICATION-WITH-LOGIN-SUPPORT)

(def (generic e) login (application session login-data)
  (:documentation "Authenticates the current web session, if there's any.
The default implementation calls AUTHENTICATE with LOGIN-DATA and stores the return value in a potentially freshly created web session.
In case of success it returns a valid web session (potentially a freshly created one), otherwise it signals an error."))

(def (generic e) logout (application session)
  (:documentation "Make the necessary sideffects in the session to log it out. May signal an error."))

(def (generic e) authenticate (application potentially-unregistered-web-session login-data)
  (:documentation "Should return something non-NIL in case the authentication was successful. The framework treats the return value as a mere generalized boolean, but it also stores it in the AUTHENTICATE-RETURN-VALUE slot of the web session for later use by user code."))

(def (generic e) is-logged-in? (session)
  (:documentation "Should return T if we are in an authenticated session opened by a succesful LOGIN call.")
  (:method (session)
    #f))

;;;;;;
;;; Authorization

(def (generic e) authorize-operation (application form)
  (:method (application form)
    #t))
