;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (function o) find-action-from-request (frame)
  (bind ((action-id (parameter-value +action-id-parameter-name+)))
    (when action-id
      (app.debug "Found action-id parameter ~S, looking it up in frame ~A" action-id frame)
      (bind ((action (gethash action-id (action-id->action-of frame))))
        (if action
            (progn
              (app.debug "Looked up as valid action ~A" action)
              (values action action-id))
            (values))))))

(def (class* e) action (string-id-for-funcallable-mixin)
  ()
  (:metaclass funcallable-standard-class))

(def (macro e) make-action (&body body)
  (with-unique-names (action)
    `(bind ((,action (make-instance 'action)))
       (set-funcallable-instance-function ,action (named-lambda action-body ()
                                                    ,@body))
       ,action)))

;; don't call these make-foo because that would be misleading. it's not only about making, but also
;; about registering the actions, and the time the registering happens is quite important. leave
;; the make-foo nameing convention for sideffect-free stuff...
(def (macro e) action/uri ((&key scheme path application-relative-path delayed-content) &body body)
  `(register-action/uri (make-action ,@body) :scheme ,scheme :path ,path :application-relative-path ,application-relative-path
                        :delayed-content ,delayed-content))

(def (macro e) action/href ((&key scheme delayed-content) &body body)
  `(hu.dwim.uri:print-uri-to-string
    (action/uri (:scheme ,scheme :delayed-content ,delayed-content)
      ,@body)))

(def function register-action (frame action)
  (assert frame)
  (assert-session-lock-held (session-of frame))
  (bind ((action-id->action (action-id->action-of frame))
         (action-id (insert-with-new-random-hash-table-key action-id->action action +action-id-length+)))
    (setf (id-of action) action-id)
    (app.dribble "Registered action with id ~S in frame ~A" action-id frame))
  action)

(def (function e) register-action/uri (action &key scheme path application-relative-path delayed-content)
  (bind ((uri (clone-request-uri)))
    (register-action *frame* action)
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (decorate-uri uri *frame*)
    (decorate-uri uri action)
    (when scheme
      (setf (hu.dwim.uri:scheme-of uri) scheme))
    (when application-relative-path
      (when path
        (error "REGISTER-ACTION/URI was called woth both PATH, and APPLICATION-RELATIVE-PATH arguments at the same time"))
      (setf (hu.dwim.uri:path-of uri) (path-of *application*))
      (hu.dwim.uri:append-path uri application-relative-path))
    (when path
      (setf (hu.dwim.uri:path-of uri) path))
    (setf (hu.dwim.uri:query-parameter-value uri +delayed-content-parameter-name+)
          (if delayed-content "t" nil))
    uri))

(def (function e) register-action/href (action &key scheme path application-relative-path delayed-content)
  (hu.dwim.uri:print-uri-to-string
   (register-action/uri action :scheme scheme :path path :application-relative-path application-relative-path
                        :delayed-content delayed-content)))

(def (generic e) decorate-uri (uri thing)
  (:method progn (uri thing)
    ;; nop
    )
  (:method progn (uri (application application))
    (unless (hu.dwim.uri:scheme-of uri)
      (setf (hu.dwim.uri:scheme-of uri) (default-uri-scheme-of application)))
    (setf (hu.dwim.uri:path-of uri) (path-of application)))
  (:method progn (uri (frame frame))
    (setf (hu.dwim.uri:query-parameter-value uri +frame-id-parameter-name+) (id-of frame))
    (setf (hu.dwim.uri:query-parameter-value uri +frame-index-parameter-name+) (frame-index-of frame)))
  (:method progn (uri (action action))
    (setf (hu.dwim.uri:query-parameter-value uri +action-id-parameter-name+) (id-of action))
    (setf (hu.dwim.uri:query-parameter-value uri +frame-index-parameter-name+) (next-frame-index-of *frame*)))
  (:method-combination progn))

(def special-variable *action-js-event-handlers*)

(appendf *xhtml-body-environment-wrappers* '(action-js-event-handlers/wrapper))

(def function action-js-event-handlers/wrapper (thunk)
  (bind ((*action-js-event-handlers* '()))
    (multiple-value-prog1
        (funcall thunk)
      (when *action-js-event-handlers*
        (labels ((serialize-action-arguments (arguments)
                   (with-output-to-string (*standard-output*)
                     (macrolet ((with-array-wrapping (&body body)
                                  `(progn
                                     (write-char #\[)
                                     ,@body
                                     (write-char #\])))
                                (write-small-js-boolean (x)
                                  `(write-char (if ,x #\1 #\0)))
                                (write-js-string (x &key escape)
                                  `(progn
                                     (write-char #\')
                                     (write-string ,(if escape
                                                        `(escape-as-js-string ,x)
                                                        x))
                                     (write-char #\'))))
                       (with-array-wrapping
                         ;; emit the action arguments in the order the js side expects them, and only emit
                         ;; them while they differ from the default value to minimize output.
                         ;; TODO it's time for some macrology here...
                         (iter (for (id href subject-dom-node one-shot event-name ajax stop-event send-client-state sync) :in arguments)
                               (for sync/defaults? = sync)
                               (for send-client-state/defaults? = (and sync/defaults?
                                                                       send-client-state))
                               (for stop-event/defaults? = (and send-client-state/defaults?
                                                                stop-event))
                               (for ajax/defaults? = (and stop-event/defaults?
                                                          ajax))
                               (for event-name/defaults? = (and ajax/defaults?
                                                                (equal event-name "onclick")))
                               (for one-shot/defaults? = (and event-name/defaults?
                                                              (not one-shot)))
                               (unless (first-time-p)
                                 (write-char #\,))
                               (with-array-wrapping
                                 ;; id
                                 (etypecase id
                                   (cons
                                    (with-array-wrapping
                                      (iter (for el :in id)
                                            (unless (first-time-p)
                                              (write-char #\,))
                                            (write-js-string el))))
                                   (string
                                    (write-js-string id)))
                                 ;; href
                                 (write-char #\,)
                                 (write-js-string href :escape #t)
                                 ;; subject-dom-node
                                 (write-char #\,)
                                 (if subject-dom-node
                                     (write-js-string subject-dom-node)
                                     (write-string "null"))
                                 (unless one-shot/defaults?
                                   ;; one-shot
                                   (write-char #\,)
                                   (write-small-js-boolean one-shot)
                                   (unless event-name/defaults?
                                     ;; event-name
                                     (write-char #\,)
                                     (write-js-string event-name)
                                     ;; ajax
                                     (unless ajax/defaults?
                                       (write-char #\,)
                                       (write-small-js-boolean ajax)
                                       (unless stop-event/defaults?
                                         ;; stop-event
                                         (write-char #\,)
                                         (write-small-js-boolean stop-event)
                                         (unless send-client-state/defaults?
                                           ;; send-client-state
                                           (write-char #\,)
                                           (write-small-js-boolean send-client-state)
                                           (unless sync/defaults?
                                             ;; sync
                                             (write-char #\,)
                                             (write-small-js-boolean sync))))))))))))))
          (bind ((serialized-arguments (serialize-action-arguments (nreverse *action-js-event-handlers*))))
            ;; keep it a simple `js not `js-onload so that we don't 'pollute' that array with another huge one
            `js(on-load
                (hdws.io.connect-action-event-handlers ,(make-string-quasi-quote nil serialized-arguments)))))))))

(def function render-action-js-event-handler (event-name id action &key action-arguments js subject-dom-node
                                                         (ajax (typep action 'action)) (stop-event #t)
                                                         (send-client-state #t) (one-shot #f)
                                                         (sync #t))
  "SUBJECT-DOM-NODE is used as a hint for the ajax progress indication."
  (check-type ajax boolean)
  (check-type subject-dom-node (or null string))
  (assert (or action js) () "~S needs either an action or a custom js" 'render-action-js-event-handler)
  ;; TODO FIXME there used to be a (when (dojo.byId ,id) ...) wrapping around the hdws.io.action call with the following comment:
  ;; KLUDGE: this condition prevents firing obsolete actions, they are not necessarily
  ;;         removed by destroy when simply replaced by some other content, this may leak memory on the cleint side
  ;; in the refactor it was deleted due to some headaches... reinstate if neccessary.
  (flet ((make-constant-form (value)
           (check-type value string)
           (make-instance 'hu.dwim.walker:constant-form :value value)))
    (bind ((href (etypecase action
                   (null            nil)
                   (action          (apply 'register-action/href action action-arguments))
                   (hu.dwim.uri:uri (hu.dwim.uri:print-uri-to-string action)))))
      ;; TODO the branches of this if should either be in two separate functions, or an assert should be added for ignored &key arguments in the true branch (ajax, send-client-state, subject-dom-node, sync)
      (if js
          `js-onload(hdws.io.connect-action-event-handler
                     ,(etypecase id
                        (cons   (make-array-form (mapcar #'make-constant-form id)))
                        (string id))
                     ,event-name
                     (lambda (event connection)
                       ,(apply js (when href (list href))))
                     :one-shot ,one-shot
                     :stop-event ,stop-event)
          ;; we delay rendering standard event handlers and send them down in one go as a big array which is processed by the client js code.
          (progn
            (push (list id href subject-dom-node one-shot event-name ajax stop-event send-client-state sync)
                  *action-js-event-handlers*)
            ;; we need to keep qq contract here regarding the return value...
            (values))))))

;; TODO this is broken
(def (macro e) js-to-lisp-rpc (&environment env &body body)
  (bind ((walked-body (hu.dwim.walker:walk-form `(progn ,@body) :environment (hu.dwim.walker:make-walk-environment env)))
         (free-variable-references (hu.dwim.walker:collect-variable-references walked-body :type 'hu.dwim.walker:free-variable-reference-form))
         (variable-names (remove-duplicates (mapcar [hu.dwim.walker:name-of !1]
                                                    free-variable-references)))
         (query-parameters (mapcar [unique-js-name (string-downcase !1)]
                                   variable-names)))
    ` `js-inline(hdws.io.xhr-post
                 ;; TODO follow keywordification of xhr-post
                  (create
                   :content (create ,@(list ,@(iter (for variable-name :in variable-names)
                                                    (for query-parameter :in query-parameters)
                                                    (collect `(quote ,query-parameter))
                                                    (collect `js-inline(.toString ,variable-name)))))
                   :url ,(action/href (:delayed-content #t)
                           (with-request-parameters ,(mapcar [list !1 !2]
                                                             variable-names
                                                             query-parameters)
                             ,@body))
                   :load (lambda (response args)
                           ;; TODO process the return value, possible ajax replacements, etc
                           )))))
