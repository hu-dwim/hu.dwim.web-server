;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (with-macro* eo) with-session-logic (&key ensure-session (requires-valid-session #t) (lock-session #t))
  (assert (and (boundp '*application*)
               *application*)
          () "May not use WITH-SESSION-LOGIC outside the dynamic extent of an application; ~S is not bound properly or NIL" '*application*)
  (assert (and (boundp '*session*)
               (boundp '*frame*))
          () "May not use WITH-SESSION-LOGIC outside the dynamic extent of an application; ~S and/or ~S is not bound properly" '*session* '*frame*)
  (bind ((application *application*)
         (session nil)
         (session-instance nil)
         (session-cookie-exists? #f)
         (invalidity-reason nil)
         (new-session? #f)
         (application-lock-held? #f))
    (app.debug "WITH-SESSION-LOGIC speaking, request is delayed-content? ~A, ajax-aware? ~A" *delayed-content-request* *ajax-aware-request*)
    (assert (not (is-lock-held? (lock-of application))))
    ;; KLUDGE this kind of locking logic can not be done with bordeaux-threads...
    (flet ((lock-application ()
             (bind ((mutex (lock-of application)))
               (sb-sys:without-interrupts
                 (unless (eq (sb-thread::mutex-%owner mutex) sb-thread:*current-thread*)
                   (sb-sys:allow-with-interrupts
                     (sb-thread:get-mutex mutex)
                     (setf application-lock-held? #t))))))
           (ensure-application-is-unlocked ()
             (sb-sys:without-interrupts
               (when application-lock-held?
                 (sb-thread:release-mutex (lock-of application))
                 (setf application-lock-held? #f)))))
      (unwind-protect
           (progn
             (lock-application)
             (progn
               ;; find the session while the app is locked, or create a new one if requested by the caller
               (setf (values session session-cookie-exists? invalidity-reason session-instance)
                     (find-session-from-request application))
               (when (and (not session)
                          (eq invalidity-reason :nonexistent)
                          ensure-session)
                 (setf session (make-new-session application))
                 (register-session application session)
                 (setf new-session? #t))
               (when (or (null session)
                         (not lock-session))
                 ;; if there's no session or we were not asked to lock it, then release the app early to lower contention
                 (ensure-application-is-unlocked))
               (setf *session* session))
             (abort-request-unless-still-valid)
             (if session
                 (bind ((local-time:*default-timezone* (client-timezone-of session)))
                   (incf (requests-to-sessions-count-of application))
                   (restart-case
                       (bind ((response (if lock-session
                                            (progn
                                              (app.debug "WITH-SESSION-LOGIC is locking session ~A as requested" session)
                                              ;; TODO check if locking would hang, handle the situation somehow
                                              (with-lock-held-on-session (session)
                                                (ensure-application-is-unlocked)
                                                (when (is-request-still-valid?)
                                                  (call-in-application-environment application session #'-body-))))
                                            (progn
                                              (app.debug "WITH-SESSION-LOGIC is NOT locking session ~A, it wasn't requested" session)
                                              (call-in-application-environment application session #'-body-)))))
                         (when (and new-session?
                                    response)
                           (decorate-session-cookie application response))
                         response)
                     (delete-current-session ()
                       :report (lambda (stream)
                                 (format stream "Delete session ~A and rety handling the request" session))
                       (mark-expired session)
                       (invoke-retry-handling-request-restart))))
                 (if (or requires-valid-session
                         (not (eq invalidity-reason :nonexistent)))
                     (bind ((response (handle-request-to-invalid-session application session invalidity-reason)))
                       (decorate-session-cookie application response)
                       response)
                     (call-in-application-environment application nil #'-body-))))
        (ensure-application-is-unlocked)))))

(def (with-macro* eo) with-frame-logic (&key (requires-valid-frame #t) (ensure-frame #f))
  (assert (and *application* *session* (boundp '*frame*)) () "May not use WITH-FRAME-LOGIC without a proper session in the environment")
  (app.debug "WITH-FRAME-LOGIC speaking, requires-valid-frame ~A, ensure-frame ~A" requires-valid-frame ensure-frame)
  (bind ((application *application*)
         (session *session*)
         ((:values frame nil invalidity-reason frame-instance) (when session
                                                                 (find-frame-for-request session))))
    (setf *frame* frame)
    (app.debug "WITH-FRAME-LOGIC looked up frame ~A from session ~A" frame session)
    (if frame
        (-body-)
        (cond
          ((and requires-valid-frame
                (not frame)
                (or (not ensure-frame)
                    (not *session*)))
           (handle-request-to-invalid-frame application session frame-instance invalidity-reason))
          ((and ensure-frame
                *session*)
           ;; set up a new frame and fall through to the entry points to set up to their favour
           (setf frame (make-new-frame application session))
           (register-frame application session frame)
           (setf *frame* frame)
           (make-redirect-response-with-frame-parameters-decorated))
          (t
           (-body-))))))

(def (with-macro* eo) with-action-logic (&key requires-valid-action)
  (app.debug "WITH-ACTION-LOGIC speaking")
  (assert (and *application* *session* *frame*) () "May not use WITH-ACTION-LOGIC without a proper application/session/frame dynamic environment")
  (bind ((application *application*)
         (session *session*)
         (frame *frame*))
    (assert-session-lock-held session)
    ;; TODO here? find its place...
    (notify-activity session)
    (labels ((convert-to-primitive-response* (response)
               (app.debug "Calling CONVERT-TO-PRIMITIVE-RESPONSE for ~A while still inside the WITH-LOCK-HELD-ON-SESSION's and WITH-ACTION-LOGIC's dynamic scope" response)
               (convert-to-primitive-response response)))
      (if frame
          (restart-case
              (progn
                (notify-activity frame)
                (process-client-state-sinks frame (query-parameters-of *request*))
                (bind ((action (find-action-from-request frame))
                       (*action* action)
                       (incoming-frame-index (parameter-value +frame-index-parameter-name+))
                       (current-frame-index (frame-index-of frame))
                       (next-frame-index (next-frame-index-of frame)))
                  (with-error-log-decorators ((make-error-log-decorator
                                                (format t "~%Action: ~A" action))
                                              #+sbcl
                                              (make-error-log-decorator
                                                (format t "~%Action source location: ~S"
                                                        (when action
                                                          (swank-backend:find-source-location (sb-kernel:funcallable-instance-fun action))))))
                    (unless (stringp current-frame-index)
                      (setf current-frame-index (integer-to-string current-frame-index)))
                    (unless (stringp next-frame-index)
                      (setf next-frame-index (integer-to-string next-frame-index)))
                    (app.debug "Incoming frame-index is ~S, current is ~S, next is ~S, action is ~A" incoming-frame-index current-frame-index next-frame-index action)
                    (cond
                      ((and action
                            incoming-frame-index)
                       (bind ((original-frame-index nil))
                         (unwind-protect-case ()
                             (if (equal incoming-frame-index next-frame-index)
                                 (progn
                                   (app.dribble "Found an action and frame is in sync...")
                                   (unless *delayed-content-request*
                                     (setf original-frame-index (step-to-next-frame-index frame)))
                                   (app.debug "Calling the action now...")
                                   (bind ((response (call-action application session frame action)))
                                     (app.dribble "Action returned response ~A" response)
                                     (when (typep response 'response)
                                       (return-from with-action-logic
                                         (convert-to-primitive-response* response)))))
                                 (return-from with-action-logic
                                   (convert-to-primitive-response* (handle-request-to-invalid-frame application session frame :out-of-sync))))
                           (:abort
                            ;; TODO the problem at hand is this: when the app specific error handler is called the stack is not yet unwinded
                            ;; so this REVERT-STEP-TO-NEXT-FRAME-INDEX is not yet called, therefore the page it renders will point to an invalid
                            ;; frame index after this unwind block is executed.
                            ;; but on the other hand without this uwp, the "retry rendering this request" restart is broken...
                            ;; FIXME we chose the lesser badness here and don't do the revert, so break the restart instead of the user visible error page
                            #+nil
                            (when original-frame-index
                              (revert-step-to-next-frame-index frame original-frame-index))))))
                      (incoming-frame-index
                       (unless (equal incoming-frame-index current-frame-index)
                         (return-from with-action-logic
                           (convert-to-primitive-response*
                            (handle-request-to-invalid-frame application session frame :out-of-sync)))))
                      ;; at the time the frame is first registered, there's no frame index param in the url, so just fall through here and
                      ;; end up at the entry points.
                      )
                    (app.dribble "Action logic fell through, proceeding to the body thunk...")
                    (if requires-valid-action
                        (handle-request-to-invalid-action application session frame action :nonexistent)
                        (values (convert-to-primitive-response* (-body-)))))))
            (delete-current-frame ()
              :report (lambda (stream)
                        (format stream "Delete frame ~A" frame))
              (mark-expired frame)
              (invoke-retry-handling-request-restart)))
          (handle-request-to-invalid-frame application session frame :nonexistent)))))

;;;;;;
;;; invalid request handling

;; TODO search all usages of this, and factor out what makes sense
(def macro emit-response-for-ajax-aware-client (() &body body)
  `(emit-http-response ((+header/status+       +http-ok+
                         +header/content-type+ +xml-mime-type+))
     <ajax-response
       <result "success">
       ,@,@body>))

;; TODO this name is so-so. names around making responses should be thought over...
(def macro make-functional-response/ajax-aware-client (() &body body)
  `(make-raw-functional-response ()
     (emit-response-for-ajax-aware-client ()
       ,@body)))

(def function handle-delayed-request-to-invalid-session/frame/action ()
  ;; what else can we do? it's a *delayed-content-request* not a full page reload...
  (make-do-nothing-response))

(def method handle-request-to-invalid-session ((application application) session invalidity-reason)
  (app.debug "Default HANDLE-REQUEST-TO-INVALID-SESSION is speaking, invalidity-reason is ~S, *ajax-aware-request* is ~S" invalidity-reason *ajax-aware-request*)
  (cond
    (*ajax-aware-request*
     (make-functional-response/ajax-aware-client ()
       <script `js-inline(wui.inform-user-about-error "error.ajax.request-to-invalid-session")>))
    ((not *delayed-content-request*)
     (app.debug "Default HANDLE-REQUEST-TO-INVALID-SESSION is sending a redirect response to ~A" application)
     (make-redirect-response-for-current-application))
    (t
     (handle-delayed-request-to-invalid-session/frame/action))))

(def method handle-request-to-invalid-frame ((application application) session frame invalidity-reason)
  (app.debug "Default HANDLE-REQUEST-TO-INVALID-FRAME speaking, invalidity-reason is ~S, *ajax-aware-request* is ~S" invalidity-reason *ajax-aware-request*)
  (cond
    (*ajax-aware-request*
     (make-functional-response/ajax-aware-client ()
       <script `js-inline(wui.inform-user-about-error "error.ajax.request-to-invalid-frame")>))
    ((not *delayed-content-request*)
     (app.debug "Default HANDLE-REQUEST-TO-INVALID-FRAME is sending a redirect response to ~A" application)
     (make-redirect-response-for-current-application))
    (t
     (handle-delayed-request-to-invalid-session/frame/action))))

(def method handle-request-to-invalid-action ((application application) session frame action invalidity-reason)
  (app.debug "Default HANDLE-REQUEST-TO-INVALID-ACTION speaking, invalidity-reason is ~S, *ajax-aware-request* is ~S" invalidity-reason *ajax-aware-request*)
  (cond
    (*ajax-aware-request*
     (make-functional-response/ajax-aware-client ()
       <script `js-inline(wui.inform-user-about-js-error)>))
    ((not *delayed-content-request*)
     (app.debug "Default HANDLE-REQUEST-TO-INVALID-ACTION is sending a redirect response to ~A" application)
     (make-redirect-response-for-current-application))
    (t (handle-delayed-request-to-invalid-session/frame/action))))

(def (function e) invoke-delete-current-frame-restart ()
  (invoke-restart (find-restart 'delete-current-frame)))

(def (function e) invoke-delete-current-session-restart ()
  (invoke-restart (find-restart 'delete-current-session)))

(def method register-frame ((application application) (session session) (frame frame))
  (assert-session-lock-held session)
  (assert (null (session-of frame)) () "The frame ~A is already registered to a session" frame)
  (assert (or (not (boundp '*frame*))
              (null *frame*)
              (eq *frame* frame)))
  (bind ((frame-id->frame (frame-id->frame-of session)))
    ;; TODO purge frames
    (bind ((frame-id (insert-with-new-random-hash-table-key frame-id->frame frame +frame-id-length+)))
      (app.debug "Registered frame ~A with id ~S" frame frame-id)
      (setf (id-of frame) frame-id)
      (setf (session-of frame) session)
      frame)))

(def method register-session ((application application) (session session))
  (assert-application-lock-held application)
  (assert (null (application-of session)) () "The session ~A is already registered to an application" session)
  (assert (or (not (boundp '*session*))
              (null *session*)
              (eq *session* session)))
  (bind ((session-id->session (session-id->session-of application)))
    (when (> (hash-table-count session-id->session)
             (maximum-sessions-count-of application))
      (too-many-sessions application))
    (bind ((session-id (insert-with-new-random-hash-table-key session-id->session session +session-id-length+)))
      (setf (id-of session) session-id)
      (setf (application-of session) application)
      (app.dribble "Registered session with id ~S" (id-of session))
      session)))

(def method delete-session ((application application) (session session))
  (assert-application-lock-held application)
  (assert (eq (application-of session) application))
  (app.dribble "Deleting session ~A" session)
  (bind ((session-id->session (session-id->session-of application)))
    (remhash (id-of session) session-id->session))
  (values))

(def method purge-sessions :around (application)
  (with-thread-name " / PURGE-SESSIONS"
    (call-next-method)))

(def (method o) purge-sessions ((application application))
  (app.dribble "Purging the sessions of ~S" application)
  ;; this method must be called while not holding any session or application lock
  (assert (not (is-lock-held? (lock-of application))) () "You must NOT have a lock on the application when calling PURGE-SESSIONS (or on any of its sessions)!")
  (setf (sessions-last-purged-at-of application) (get-monotonic-time))
  (bind ((deleted-sessions (list))
         (live-sessions (list))
         (deadline-timeout 1))
    (with-lock-held-on-application (application)
      (iter (for (session-id session) :in-hashtable (session-id->session-of application))
            (cond
              ((is-session-alive? session)
               (push session live-sessions))
              ((sb-thread:mutex-owner (lock-of session))
               ;; if someone has a lock on the session (e.g. a zombie request running on CPU so long that the session under it times out meanwhile), then we must skip that session because it'll lead to a deadlock...
               ;; note: it's safe to call SB-THREAD:MUTEX-OWNER here because noone may lock a session without holding the app lock, which we hold here
               (app.warn "Dead session ~A is also locked in PURGE-SESSIONS, therefore it will be excluded from this round of purging" session))
              (t
               (block deleting-session
                 (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                                 (app.error "Could not delete expired/invalid session ~A of application ~A, got error ~A" session application error))
                                               (lambda (&key &allow-other-keys)
                                                 (return-from deleting-session)))
                   (delete-session application session)
                   (push session deleted-sessions)))))))
    (with-thread-name " / calling NOTIFY-SESSION-EXPIRATION on exired sessions"
      (dolist (session deleted-sessions)
        (block noifying-session
          (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                          (app.error "Error happened while notifying session ~A of application ~A about its exiration, got error ~A" session application error))
                                        (lambda (&key &allow-other-keys)
                                          (return-from noifying-session)))
            (with-lock-held-on-session (session :deadline deadline-timeout)
              (notify-session-expiration application session))))))
    (with-thread-name " / calling PURGE-FRAMES on live sessions"
      (dolist (session live-sessions)
        (block purging-session-frames
          (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                          (app.error "Error happened while purging frames of ~A of application ~A. Got error ~A" session application error))
                                        (lambda (&key &allow-other-keys)
                                          (return-from purging-session-frames)))
            (with-lock-held-on-session (session :deadline deadline-timeout)
              (purge-frames application session))))))
    (values)))

(def (function e) mark-all-sessions-expired (application)
  (with-lock-held-on-application (application)
    (iter (for (key session) :in-hashtable (session-id->session-of application))
          (mark-expired session))))
