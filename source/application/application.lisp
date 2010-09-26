;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def localization-loader-callback wui-application-localization-loader :hu.dwim.wui "localization/application/" :log-discriminator "hu.dwim.wui.application")

(def (class* e) standard-application (application-with-login-support
                                      application-with-home-package
                                      application-with-dojo-support)
  ())

(def (class* e) application (broker-at-path-prefix
                             request-counter-mixin
                             debug-context-mixin)
  ((requests-to-sessions-count 0 :type integer :export :accessor)
   (entry-points nil)
   (default-uri-scheme "http" :type string)
   (default-locale "en" :type string)
   (supported-locales '("en") :type sequence)
   (default-timezone local-time:*default-timezone* :type local-time::timezone)
   (session-class nil :type (or null standard-class))
   (session-timeout *default-session-timeout* :type number)
   (sessions-last-purged-at (get-monotonic-time) :type number)
   (maximum-sessions-count *maximum-sessions-per-application-count* :type integer)
   (session-id->session (make-hash-table :test 'equal) :type hash-table :export :accessor)
   (administrator-email-address nil :type (or null string))
   (lock (make-recursive-lock "hu.dwim.wui application lock"))
   (running-in-test-mode #f :type boolean :accessor running-in-test-mode? :export :accessor)
   (debug-client-side :type boolean :writer (setf debug-client-side?))
   (ajax-enabled *default-ajax-enabled* :type boolean :accessor ajax-enabled?))
  (:default-initargs :path-prefix "/"))

(def (function e) make-frame-root-component (&optional content)
  (make-frame-root-component-using-application *application* *session* *frame* content))

(def method debug-on-error? ((application application) error)
  (cond
    ((slot-boundp application 'debug-on-error)
     (slot-value application 'debug-on-error))
    ;; TODO: there's some anomaly here compared to sessions/apps: sessions are owned by an application, but apps don't have a server slot. resolve or ignore? are apps exclusively owned by servers?
    ((and (boundp '*server*)
          *server*)
     (debug-on-error? *server* error))
    (t (call-next-method))))

(def method debug-client-side? ((self application))
  (if (slot-boundp self 'debug-client-side)
      (slot-value self 'debug-client-side)
      (call-next-method)))

(def (function e) human-readable-broker-path (server application)
  (bind ((path (broker-path-to-broker server application)))
    (assert (member application path))
    (apply #'string+
           (iter (for el :in path)
                 (etypecase el
                   (application (collect (path-prefix-of el)))
                   (server nil))))))

(def function broker-path-to-broker (root broker)
  (map-broker-tree root (lambda (path)
                          (when (eq (first path) broker)
                            (return-from broker-path-to-broker (nreverse path))))
                   :visit-type :path))

(def function total-web-session-count (server)
  (bind ((sum 0))
    (map-broker-tree server (lambda (el)
                              (when (typep el 'application)
                                (incf sum (hash-table-count (session-id->session-of el))))))
    sum))

(def function map-broker-tree (root visitor &key
                                    (filter (constantly #t))
                                    (visit-type :leaf))
  (check-type visit-type (member :path :leaf))
  (macrolet ((visit ()
               `(bind ((el (ecase visit-type
                             (:path path)
                             (:leaf (first path)))))
                  (when (funcall filter el)
                    (funcall visitor el))))
             (recurse (children)
               `(map nil (lambda (child)
                           (%recurse (cons child path)))
                     ,children)))
    (labels ((%recurse (path)
               (bind ((el (first path)))
                 (etypecase el
                   (broker-based-server (visit)
                                        (recurse (brokers-of el)))
                   (delegate-broker (visit)
                                    (dolist (child (children-of el))
                                      (recurse child)))
                   (application (visit)
                                (recurse (entry-points-of el)))
                   (broker (visit))
                   (null nil)))))
      (%recurse (list root))))
  (values))

(def (function i) assert-application-lock-held (application)
  (assert (is-lock-held? (lock-of application)) () "You must have a lock on the application here"))

(def (with-macro* e) with-lock-held-on-application (application)
  (debug-only*
    (iter (for (nil session) :in-hashtable (session-id->session-of application))
          (assert (not (is-lock-held? (lock-of session))) ()
                  "You are trying to lock the application ~A while one of its session ~A is already locked by you"
                  application session)))
  (multiple-value-prog1
      (progn
        (threads.dribble "Entering with-lock-held-on-application for app ~S in thread ~S" application (current-thread))
        (with-recursive-lock-held ((lock-of application))
          (-body-)))
    (threads.dribble "Leaving with-lock-held-on-application for app ~S in thread ~S" application (current-thread))))

(def (constructor o) (application path-prefix)
  (assert path-prefix)
  #*((:sbcl (setf (sb-thread:mutex-name (lock-of -self-)) (format nil "hu.dwim.wui application lock for application ~A" path-prefix)))))

(def method session-class-of :around ((self application))
  (or (call-next-method)
      (setf (session-class-of self)
            (aprog1
                (make-instance 'standard-class
                               :direct-superclasses (mapcar #'find-class (session-class self))
                               :name (symbolicate '#:session-class-for/
                                                  (class-name (class-of self))
                                                  "/"
                                                  (path-prefix-of self)))
              (app.debug "Instantiated session class ~A for application ~A" it self)))))

(def method session-class list ((application application))
  'session)

(def method make-new-session ((application application))
  (make-instance (session-class-of application)
                 :time-to-live (session-timeout-of application)))

(def method make-new-frame ((application application) (session session))
  (assert session)
  (app.debug "Creating new frame for session ~A of app ~A" session application)
  (assert-session-lock-held session)
  (make-instance 'frame
                 :time-to-live (frame-timeout-of session)))

(def function maybe-invoke-debugger/application (condition &key (context (or (and (boundp '*session*)
                                                                                  *session*)
                                                                             (and (boundp '*application*)
                                                                                  *application*)
                                                                             (and (boundp '*broker-stack*)
                                                                                  (first *broker-stack*)))))
  (maybe-invoke-debugger condition :context context))

;;;;;;
;;; application-with-home-package

(def (class* e) application-with-home-package (application)
  ((home-package *package* :type package)))

(def method call-in-application-environment :around ((application application-with-home-package) session thunk)
  (bind ((*package* (home-package-of application)))
    (call-next-method)))

;;;;;;
;;; application-with-layer

(def (class* e) application-with-layer (application)
  ((layer)))

(def method call-in-application-environment :around ((application application-with-layer) session thunk)
  (funcall-with-layer-context (adjoin-layer (layer-of application)
                                            (current-layer-context))
                              #'call-next-method))
