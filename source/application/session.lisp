;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def class* activity-monitor-mixin ()
  ((last-activity-at (get-monotonic-time) :type number)
   (last-activity-timestamp (local-time:now) :type local-time:timestamp)
   (time-to-live :type number)))

(def (generic e) notify-activity (thing)
  (:method ((self activity-monitor-mixin))
    (setf (last-activity-at-of self) (get-monotonic-time))
    (setf (last-activity-timestamp-of self) (local-time:now))))

(def (generic e) mark-expired (thing)
  (:method ((thing activity-monitor-mixin))
    (setf (last-activity-at-of thing) most-negative-fixnum)))

(def (generic e) time-since-last-activity (thing)
  (:method ((self activity-monitor-mixin))
    (- (get-monotonic-time) (last-activity-at-of self))))

(def generic is-timed-out? (thing)
  (:method ((self activity-monitor-mixin))
    (> (time-since-last-activity self)
       (time-to-live-of self))))

(def class* string-id-mixin ()
  ((id nil :type string)))

(def class* string-id-for-funcallable-mixin ()
  ((id nil :type string))
  (:metaclass funcallable-standard-class))

(def print-object (string-id-mixin :identity #t :type #f)
  (print-object-for-string-id-mixin -self-))

(def print-object (string-id-for-funcallable-mixin :identity #t :type #f)
  (print-object-for-string-id-mixin -self-))

(def function print-object-for-string-id-mixin (self)
  (write-string (string (class-name (class-of self))))
  (write-string " ")
  (aif (id-of self)
       (write-string it)
       "<no id yet>"))


;;;;;;
;;; Session

(def class* session (string-id-mixin
                     activity-monitor-mixin
                     debug-context-mixin)
  ((http-user-agent (identify-http-user-agent *request*) :type http-user-agent)
   (application nil :type application)
   (client-timezone (default-timezone-of *application*) :type local-time::timezone)
   (frame-timeout *default-frame-timeout* :type integer)
   (frame-id->frame (make-hash-table :test 'equal) :type hash-table)
   (lock (make-recursive-lock "hu.dwim.web-server session lock"))
   (computed-universe nil)
   (valid #t :type boolean :accessor is-session-valid? :export :accessor)))

(def method debug-on-error? ((session session) error)
  (if (slot-boundp session 'debug-on-error)
      (slot-value session 'debug-on-error)
      (debug-on-error? (application-of session) error)))

(def (function e) mark-session-invalid (&optional (session *session*))
  (setf (is-session-valid? session) #f)
  (maphash-values #'mark-frame-invalid (frame-id->frame-of session)))

(def function is-session-alive? (session)
  (cond
    ((not (is-session-valid? session)) (values #f :invalidated))
    ((is-timed-out? session) (values #f :timed-out))
    (t (values #t))))

(def (with-macro* e) with-lock-held-on-session (session &key deadline)
  (with-lock-held-on-thing ('session session :deadline deadline)
    (-with-macro/body-)))

(def method (setf id-of) :before (id (session session))
  (awhen (id-of session)
    (error "The session ~S already has an id: ~A." session it))
  #*((:sbcl (setf (sb-thread:mutex-name (lock-of session)) (format nil "hu.dwim.web-server session lock for session ~A" id)))))

(def (function i) assert-session-lock-held (session)
  (assert (is-lock-held? (lock-of session)) () "You must have a lock on the session here"))

(def (generic e) notify-session-expiration (application session)
  (:method (application (session session))
    ;; nop
    ))

(def function find-session-by-id (application session-id &key (otherwise :error otherwise?))
  (check-type session-id string)
  (or (gethash session-id (session-id->session-of application))
      (handle-otherwise
        (error "Could not find session ~S of application ~A" session-id application))))

(def (function o) find-session-from-request (application)
  ;; TODO ? deal with situations when there are multiple cookies returned due to e.g. some having been installed on overlapping erroneous paths
  (bind ((session-id (cookie-value +session-cookie-name+))
         (cookie-exists? (not (null session-id)))
         (session nil)
         (session-instance nil)
         (invalidity-reason nil))
    (app.dribble "Looking for session-id cookie ~S among ~A" +session-cookie-name+ (cookies-of *request*))
    (when session-id
      (app.debug "Found session-id parameter ~S" session-id)
      (setf session-instance (find-session-by-id application session-id :otherwise nil))
      (setf session session-instance)
      (when session
        (bind ((alive?))
          (setf (values alive? invalidity-reason) (is-session-alive? session))
          (if alive?
              (app.debug "Looked up as valid, alive session ~A" session)
              (progn
                (app.debug "Looked up as a session, but it's not valid anymore due to ~S. It's ~A." invalidity-reason session)
                (setf session nil))))))
    (when (and (not session)
               (not invalidity-reason))
      (setf invalidity-reason :nonexistent))
    (values session cookie-exists? invalidity-reason session-instance)))

;;;;;;
;;; http user agent breakdown

;; TODO rename to what? http-user-agent-statistics? breakdown means something else...
;; TODO or just implement it in a different way than inheritance... also including other statistics than the count...
(def class* http-user-agent-breakdown (http-user-agent)
  ((count :type integer)))

(def function make-http-user-agent-breakdown (&optional (server *server*))
  (bind ((user-agents (make-hash-table :test #'equal))
         (applications '()))
    (map-broker-tree server
                     [push !1 applications]
                     :filter (of-type 'application))
    (dolist (application applications)
      (iter (for (id session) :in-hashtable (session-id->session-of application))
            (for user-agent = (http-user-agent-of session))
            (when user-agent
              (incf (gethash (raw-http-header-value-of user-agent) user-agents 0)))))
    (iter (for (header-value count) :in-hashtable user-agents)
          (collect (change-class (parse-http-user-agent header-value) 'http-user-agent-breakdown :count count)))))
