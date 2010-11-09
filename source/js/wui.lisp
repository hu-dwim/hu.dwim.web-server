;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(log.debug "Started evaluating wui.js")

(dojo.getObject "wui" #t)
(dojo.getObject "wui.io" #t)
(dojo.getObject "wui.i18n" #t)
(dojo.getObject "wui.field" #t)
(dojo.getObject "wui.help" #t)

(defun wui.connect (objects event function)
  (assert objects "wui.connect called with nil object")
  (assert function "wui.connect called with nil function")
  (unless dojo.config.isDebug
    (bind ((original-function function))
      (setf function (lambda ()
                       (try
                            (.apply original-function this arguments)
                         (catch (e)
                           (wui.inform-user-about-error "error.generic-javascript-error")))))))
  (flet ((lookup (id)
           (bind ((result (dojo.byId id)))
             (assert result "lookup of the dom node " id " in wui.connect failed")
             (return result))))
    (if (dojo.isArray object)
        (dolist (object objects)
          (dojo.connect (lookup object) event function))
        (return (dojo.connect (lookup objects) event function)))))

(defun wui.disconnect (connection)
  (assert connection "wui.disconnect called with nil connection")
  (dojo.disconnect connection))

(defun wui.maybe-invoke-debugger ()
  ;; doesn't work in chrome? (apply window.console.error arguments)
  (when dojo.config.isDebug
    debugger))

(defun wui.map (fn array)
  (do ((i 0 (1+ i)))
      ((>= i array.length))
    (setf (aref array i) (fn (aref array i))))
  (return array))

(defun wui.map-child-nodes (node visitor)
  ;; NOTE: some usages if this function are due to the fact that in some cases node.childNodes does not work in IE
  (let ((children (array))
        (child node.firstChild))
    ;; NOTE: copy the children this way, so that IE is happy
    (while child
      (children.push child)
      (setf child child.nextSibling))
    (dolist (child children)
      (visitor child))
    (return children)))

(defun wui.shallow-copy (object)
  (return (dojo.mixin (create) object)))

(defun wui.append-query-parameter (url name value)
  (setf url (+ url
               (if (< (.index-of url "?") 0)
                   "?"
                   "&")
               (encodeURIComponent name)
               "="
               (encodeURIComponent value)))
  (return url))

(defun wui.decorate-url-with-modifier-keys (url event)
  (when event.shiftKey
    (setf url (wui.append-query-parameter url
                                          #.(escape-as-uri +shitf-key-parameter-name+)
                                          "t")))
  (when event.ctrlKey
    (setf url (wui.append-query-parameter url
                                          #.(escape-as-uri +control-key-parameter-name+)
                                          "t")))
  (when (or event.altKey
            event.metaKey)
    (setf url (wui.append-query-parameter url
                                          #.(escape-as-uri +alt-key-parameter-name+)
                                          "t")))
  (return url))

#+nil ; currently unused
(defun wui.decorate-url-with-frame-and-action (url (frame-id wui.frame-id) (frame-index wui.frame-index) action-id)
  (setf url (+ url (if (< (.index-of url "?") 0)
                       "?"
                       "&")))
  (setf url (+ url #.(escape-as-uri +frame-id-parameter-name+)    "=" (encodeURIComponent frame-id)
               "&" #.(escape-as-uri +frame-index-parameter-name+) "=" (encodeURIComponent frame-index)))
  (when action-id
    (setf url (+ url "&" #.(escape-as-uri +action-id-parameter-name+) "=" (encodeURIComponent action-id))))
  (return url))

(defun wui.absolute-url-from (url)
  (return
    (if (> (.index-of url ":") 0) ; KLUDGE this test is fragile here
        url
        (+ window.location.protocol "//"
           window.location.hostname ":"
           window.location.port
           url))))

;; this is a condition object, even if it looks like a function...
(defun wui.communication-error (message)
  (setf this.type "wui.communication-error")
  (setf this.message message))

(defun wui.inform-user-about-js-error ()
  (wui.inform-user-about-error "error.generic-javascript-error" :title "error.generic-javascript-error.title"))

;; TODO factor out dialog code?
(defun wui.inform-user-about-error (message &key (title "error.generic-javascript-error.title"))
  (log.debug "Informing user about error, message is '" message "', title is '" title "'")
  (dojo.require "dijit.Dialog")
  (setf message (wui.i18n.localize message))
  (setf title (wui.i18n.localize title))
  (bind ((dialog (new dijit.Dialog (create :title title)))
         (reload-button (new dijit.form.Button (create :label #"action.reload-page")))
         (cancel-button (new dijit.form.Button (create :label #"action.cancel"))))
    (.placeAt (new dijit.layout.ContentPane (create :content message)) dialog.containerNode)
    ;; TODO add a 'float: right' or equivalent to the container of the buttons
    (reload-button.placeAt dialog.containerNode)
    (cancel-button.placeAt dialog.containerNode)
    (wui.connect reload-button "onClick" (lambda ()
                                           (window.location.reload)))
    (wui.connect cancel-button "onClick" (lambda ()
                                           (dialog.hide)
                                           (dialog.destroyRecursive)))
    (dialog.show)))

;;;;;;
;;; io

(setf wui.io.sync-ajax-action-in-progress false)

;; open dojo issue about error handler not being called: http://bugs.dojotoolkit.org/ticket/10985
(defun wui.io.action (url &key on-success on-error event (ajax true) subject-dom-node (sync true) (xhr-sync false) (send-client-state true))
  (when event
    (setf url (wui.decorate-url-with-modifier-keys url event)))
  (bind ((decorated-url (wui.append-query-parameter url
                                                    #.(escape-as-uri +ajax-aware-parameter-name+)
                                                    (if ajax "t" "")))
         (form (aref document.forms 0)))
    (wui.save-scroll-position "content")
    (if ajax
        (bind ((ajax-target (dojo.byId subject-dom-node))
               (ajax-indicator-teardown nil))
          (when wui.io.sync-ajax-action-in-progress
            (log.warn "Ignoring a (wui.io.action :sync true :ajax true ...) call because there's already a pending sync action")
            (return))
          (log.debug "Will fire an ajax request, ajax-target: " ajax-target)
          (when sync
            (setf wui.io.sync-ajax-action-in-progress true))
          (flet ((xhr-finished ()
                   (when ajax-indicator-teardown
                     (ajax-indicator-teardown))
                   (when sync
                     (setf wui.io.sync-ajax-action-in-progress false))))
            (when ajax-target
              (setf ajax-indicator-teardown (wui.io.install-ajax-progress-indicator ajax-target)))
            (wui.io.xhr-post :url decorated-url
                             ;; :sync true pretty much stops the whole browser tab including animated images...
                             :sync xhr-sync
                             :form (when send-client-state
                                     form)
                             :on-error (lambda (response io-args)
                                         (xhr-finished)
                                         (wui.io.process-ajax-network-error response io-args)
                                         (when on-error
                                           (on-error)))
                             :on-success (lambda (response io-args)
                                           (xhr-finished)
                                           (wui.io.clear-ajax-replacement-markers)
                                           (wui.io.process-ajax-answer response io-args)
                                           (when on-success
                                             (on-success))))))
        (if (and send-client-state
                 form
                 (< 0 form.elements.length))
            (progn
              (setf (slot-value form 'action) decorated-url)
              (form.submit))
            (setf window.location.href decorated-url)))))

(defun wui.io.install-ajax-progress-indicator (target-node)
  (bind ((animation nil)
         (progress-node (document.create-element "div")))
    (dojo.style progress-node "opacity" 0)
    (dojo.addClass target-node "ajax-target")
    (dojo.addClass progress-node "ajax-request-in-progress")
    (dojo.contentBox progress-node (dojo.contentBox target-node))
    (dojo.place progress-node target-node "before")
    (log.debug "Fading out target-node " target-node)
    (setf animation
          (.play (dojo.fx.combine (array
                                   (dojo.animateProperty (create :node target-node
                                                                 :duration 1000
                                                                 :rate 50
                                                                 :properties (create :opacity (create :start 1 :end 0.3))))
                                   (dojo.animateProperty (create :node progress-node
                                                                 :duration 500
                                                                 :rate 50
                                                                 :properties (create :opacity (create :start 0.3 :end 1))))))))
    (return
      (lambda ()
        (.stop animation)
        ;; dojo.destroy can deal with parentNode = null (which sometimes happens maybe because the indicator dom node gets GC'd, possibly due to its parent node having been ajax-replaced?)
        (dojo.destroy progress-node)
        (dojo.removeClass target-node "ajax-target")
        (.play (wui.io.make-ajax-replacement-fade-in target-node))))))

(defun wui.io.make-ajax-replacement-fade-in (node start-opacity)
  (return
    (dojo.animateProperty
     (create :node node
             :duration 500
             :rate 50
             :properties (create :opacity (create :start (or start-opacity
                                                             (dojo.style node "opacity"))
                                                  :end 1))))))

(defun wui.io.make-action-event-handler (href &key on-success on-error subject-dom-node (ajax true)
                                         (send-client-state true) (sync true) (xhr-sync false))
  (return
    (lambda (event connection)
      (log.debug "An action event handler made by wui.io.make-action-event-handler is being invoked on event " event ", connection " connection)
      (assert event)
      (assert connection)
      (wui.io.action href
                     :event event
                     :ajax ajax
                     :sync sync
                     :xhr-sync xhr-sync
                     :on-error (when on-error
                                 (lambda ()
                                   (on-error event connection)))
                     :on-success (when on-success
                                   (lambda ()
                                     (on-success event connection)))
                     :subject-dom-node subject-dom-node
                     :send-client-state send-client-state))))

(defun wui.io.connect-action-event-handler (id event-name handler &key one-shot (stop-event true))
  (flet ((connect-one (id)
           (if ($ id)
               (bind ((connection nil))
                 (setf connection (wui.connect id event-name
                                               (lambda (event)
                                                 (when stop-event
                                                   (log.debug "Action handler on id " id ", node " (dojo.byId id) " is stopping event " event)
                                                   (dojo.stopEvent event))
                                                 (when one-shot
                                                   ;; TODO support different one-shot strategies? 1) disconnect, 2) use a captured boolean guard.
                                                   ;; 2) with stop-event could be used to hide events from covering parent nodes
                                                   (log.debug "Disconnecting one-shot event handler after firing; id " id  ", node " (dojo.byId id) ", connection " connection)
                                                   (wui.disconnect connection))
                                                 (handler event connection))))
                 (return connection))
               (progn
                 (log.error "Node not found when trying to connect event handler. Id is " id)
                 (wui.maybe-invoke-debugger)))))
    (if (dojo.isArray id)
        (dolist (one id)
          (connect-one one))
        (return (connect-one id)))))

(defun wui.io.connect-action-event-handlers (handlers)
  (flet ((to-boolean (x default-value)
           (cond
             ((= x 1) (return true))
             ((= x 0) (return false))
             (t (if (eq default-value undefined)
                    (assert false "?! we are expecting either 1 or 0 instead of " x)
                    (return default-value)))))
         (connect-one (handler)
           (bind ((id                (.shift handler))
                  (href              (.shift handler))
                  (subject-dom-node  (.shift handler))
                  (one-shot          (to-boolean (.shift handler) false))
                  (event-name        (or (.shift handler) "onclick"))
                  (ajax              (to-boolean (.shift handler) true))
                  (stop-event        (to-boolean (.shift handler) true))
                  (send-client-state (to-boolean (.shift handler) true))
                  (sync              (to-boolean (.shift handler) true)))
             (wui.io.connect-action-event-handler
              id event-name (wui.io.make-action-event-handler href
                                                              :subject-dom-node subject-dom-node
                                                              :ajax ajax
                                                              :send-client-state send-client-state
                                                              :sync sync)
              :one-shot one-shot
              :stop-event stop-event))))
    (foreach #'connect-one handlers)))

;; open dojo issue about (sometimes defaulting) node.type taking precedence over node.dojoType: http://bugs.dojotoolkit.org/ticket/10951
(defun wui.io.instantiate-dojo-widgets (widget-ids)
  (log.debug "Instantiating (and destroying previous versions of) the following widgets " widget-ids)
  (dolist (widget-id widget-ids)
    (awhen (dijit.byId widget-id)
      (.destroyRecursive it)))
  (dojo.parser.instantiate (wui.map dojo.byId widget-ids)))

(defun wui.io.lazy-context-menu-handler (event connection href id parent-id)
  ((wui.io.make-action-event-handler
    href
    :ajax true
    :subject-dom-node parent-id
    :send-client-state false
    :sync true
    :on-success (lambda (event connection)
                  (aif (dijit.byId id)
                       ;; TODO why does it ever happen that the ajax answer is empty?
                       ;; TODO don't use dojo internals. find a way to reinvoke the same event, or make sure some other way that the context menu comes up
                       (._scheduleOpen it event.target nil (create :x event.pageX :y event.pageY))
                       (log.error "Context menu was not found after processing the ajax answer (empty ajax answer?). The id we looked for is " id))))
   event connection))

(defun wui.io.postprocess-inserted-node (original-node imported-node)
  ;; this used to be needed before WITH-COLLAPSED-JS-SCRIPTS started to collect all js fragments into a toplevel script node in the ajax answer. might come handy for something later, so leave it for now...
  )

(defun wui.io.eval-script-tag (node)
  (let ((type (node.getAttribute "type")))
    (if (= type "text/javascript")
        (let ((script node.text))
          (unless (dojo.string.isBlank script)
            ;;(log.debug "Eval'ing script " (.substring script 0 128))
            (log.debug "Eval'ing script tag " node)
            (eval script)))
        (throw (+ "Script tag with unexpected type: '" type "'")))))

;; TODO implement something smarter to deal with the user clicking around on the client while the server is busy.
;; when sync is false, the user can stack up many ajax requests queueing on the server at the session lock...
(defun wui.io.xhr-post (&key url form
                        (sync false)
                        (on-error wui.io.process-ajax-network-error)
                        (on-success wui.io.process-ajax-answer)
                        (handle-as "xml"))
  ;; absolutize url if it's a relative one.
  (when url
    (setf url (wui.absolute-url-from url)))
  (bind ((params (create :url url
                         :form form
                         :sync sync
                         :handle-as handle-as
                         :error on-error
                         :load on-success)))
    (return (dojo.xhrPost params))))

(defun wui.io.process-ajax-network-error (response ioArgs)
  (log.error "wui.io.process-ajax-network-error called with response " response ", ioArgs " ioArgs)
  (bind ((network-error? ioArgs.error))
    ;; NOTE: this is a fragile way to differentiate between network errors and random js errors. http://bugs.dojotoolkit.org/ticket/6814
    (cond
      (network-error?
       (wui.inform-user-about-error "error.network-error"
                                    :title "error.network-error.title"))
      ((not dojo.config.isDebug)
       (wui.inform-user-about-error "error.generic-javascript-error"
                                    :title "error.generic-javascript-error.title")))))

(defun wui.io.import-ajax-received-xhtml-node (node)
  ;; Makes an XMLHTTP-received node suitable for inclusion in the document.
  (log.debug "Importing ajax answer node with id " (.getAttribute node "id"))
  (cond
    (dojo.isMozilla
     (return node))
    ((or dojo.isChrome dojo.isOpera dojo.isSafari)
     (return (document.importNode node true)))
    (dojo.isIE
     ;; ie is randomly dropping the script tags (m$ is as lame as usual...)
     ;; i couldn't find anything that affects the behaviour, my best guess is that it may depend
     ;; on how the script reached the browser: scripts in the original document may have
     ;; more permissions? either way, handle script tags specially to overcome it.
     (let ((result)
           (copy-attributes (lambda (from to)
                              (dolist (attribute from.attributes)
                                (let ((value attribute.node-value))
                                  (setf (aref to attribute.node-name) value))))))
       (cond ((= node.tagName "script")
              (setf result (document.createElement "script"))
              (copy-attributes node result)
              (setf result.text node.text))
             ((= node.tagName "tr")
              ;; this is ugly here for a reason: ie sucks. seems like this is the only way to create a
              ;; tr node on the ie side that behaves as normal dom nodes (i.e. it can be added to the dom).
              ;; this was tested only on ie6.
              (setf result (document.createElement "tr"))
              (copy-attributes node result)
              (dolist (td node.childNodes)
                (if (= td.tagName "td")
                    (let ((body td.xml)
                          (start (1+ (.indexOf body ">")))
                          (end (- body.length 5))
                          (imported-td (document.createElement "td")))
                      ;; chop off the opening and the closing tag, so that we get the innerHTML
                      (setf body (.substring body start end))
                      (setf imported-td.innerHTML body)
                      (copy-attributes td imported-td)
                      (result.appendChild imported-td))
                    (result.appendChild (wui.io.import-ajax-received-xhtml-node td)))))
             (t
              ;; create a node and setf its innerXML property
              ;; this will parse the xhtml we received and convert it
              ;; to dom nodes that ie will not bark on.
              (setf result (document.createElement "div"))
              (log.debug "Assigning innerHTML")
              (setf result.innerHTML node.xml)
              (log.debug "innerHTML was assigned succesfully")
              (assert (= 1 result.childNodes.length))
              (setf result result.firstChild)))
       (log.debug "Succesfully imported answer node, returning")
       (return result))))
  (log.warn "Unknown browser in import-ajax-received-xhtml-node, this will probably cause some troubles later. Browser is " navigator.userAgent)
  (return (document.importNode node true)))

;; Return a lambda that when passed a root node, will call the visitor with each of those children
;; that have the given tag-name.
(defun wui.io.make-dom-node-walker (tag-name visitor (import-node-p true) (toplevel-p false))
  (return
    (lambda (root)
      ;; NOTE: it used to be a dolist but root.childNodes does not work in IE by some weird reason
      (wui.map-child-nodes
       root
       (lambda (toplevel-node)
         (log.debug "Walking at node " toplevel-node)
         ;; node.get-elements-by-tag-name returns recursively all nodes of a document node, so that won't work here
         (when (= toplevel-node.tag-name tag-name)
           (if toplevel-p
               (let ((node toplevel-node)
                     (original-node node)
                     (id (.getAttribute node "id")))
                 (log.debug "Processing " node " with id " id)
                 (when import-node-p
                   (setf node (wui.io.import-ajax-received-xhtml-node node)))
                 (visitor node original-node))
               (progn
                 (log.debug "Will process " toplevel-node.child-nodes.length " node(s) of type '" tag-name "'")
                 ;; NOTE: it used to be a dolist but root.childNodes does not work in IE by some weird reason
                 (wui.map-child-nodes
                  toplevel-node ; create a copy and iterate on that
                  (lambda (node)
                    (when (if dojo.isIE
                              (.getAttribute node "id")
                              (slot-value node 'getAttribute))
                      (let ((original-node node)
                            (id (.getAttribute node "id")))
                        (log.debug "Processing " tag-name " node with id " id)
                        (when import-node-p
                          (setf node (wui.io.import-ajax-received-xhtml-node node)))
                        (visitor node original-node)))))))))))))

;; Returns a lambda that can be used as a dojo :load handler.  Will do some sanity checks
;; on the ajax answer, report any possible server errors, then walk the nodes with the given
;; tag-name and call the visitor on them.  If the visitor returns a node, then postprocess the
;; returned node as an added dom html fragment.
(defun wui.io.make-ajax-answer-processor (tag-name visitor (import-node-p true) (toplevel-p false))
  (let ((node-walker (wui.io.make-dom-node-walker tag-name
                                                  (lambda (node original-node)
                                                    (when (visitor node original-node)
                                                      (log.debug "Calling postprocess-inserted-node on node " node)
                                                      (wui.io.postprocess-inserted-node original-node node)))
                                                  import-node-p
                                                  toplevel-p)))
    (return
      (lambda (response args)
        (log.debug "Response is " response ", arguments are " args)
        (with-ajax-answer-logic response
          (node-walker response))))))

(setf wui.io.marked-ajax-replacements (array))

(defun wui.io.mark-ajax-replacement (node)
  (when dojo.config.isDebug
    (dojo.addClass node "ajax-replacement")
    (wui.io.marked-ajax-replacements.push node)))

(defun wui.io.clear-ajax-replacement-markers ()
  (when dojo.config.isDebug
    (dolist (node wui.io.marked-ajax-replacements)
      (dojo.removeClass node "ajax-replacement"))
    (setf wui.io.marked-ajax-replacements (array))))

(bind ((dom-replacer (wui.io.make-ajax-answer-processor
                      "dom-replacements"
                      (lambda (replacement-node)
                        (bind ((id (.getAttribute replacement-node "id"))
                               (old-node ($ id)))
                          (if old-node
                              (bind ((parent-node (slot-value old-node 'parent-node))
                                     (old-opacity (Math.min (dojo.style old-node "opacity") 0.5)))
                                (log.debug "About to replace old node with id " id)
                                (when (dojo.hasClass replacement-node "context-menu")
                                  ;; KLUDGE dijit context menu looses dom node identity, so fading cannot work on it
                                  (setf old-opacity 1))
                                (dojo.style replacement-node "opacity" old-opacity)
                                (.replace-child parent-node replacement-node old-node)
                                (wui.io.mark-ajax-replacement replacement-node)
                                (log.debug "Fading back replacement-node " replacement-node)
                                (bind ((animation (wui.io.make-ajax-replacement-fade-in replacement-node old-opacity)))
                                  (wui.connect animation "onEnd"
                                               (lambda ()
                                                 (unless (eq replacement-node (dojo.byId id))
                                                   ;; KLUDGE this should not happen, but it happens with context menus...
                                                   (log.warn "Setting opacity to 1 of orphaned replacement-node with id " id)
                                                   (dojo.style (dojo.byId id) "opacity" 1))))
                                  (.play animation))
                                (log.debug "Successfully replaced node with id " id))
                              (progn
                                (log.error "Old version of replacement node " replacement-node " with id '" id "' was not found on the client side")
                                (wui.maybe-invoke-debugger))))))))
  (setf wui.io.process-ajax-answer
        (lambda (response args)
          ;; replace some components (dom nodes)
          (log.debug "wui.io.process-ajax-answer speaking. Called with response " response ", args " args)
          (log.debug "Calling dom-replacer...")
          (dom-replacer response args)
          (log.debug "...dom-replacer returned")
          ;; look for 'script' tags and execute them with 'current-ajax-answer' bound
          (let ((script-evaluator (wui.io.make-ajax-answer-processor "script"
                                                                     (lambda (script-node)
                                                                       ;; TODO handle/assert for script type attribute
                                                                       (let ((script (dojox.xml.parser.textContent script-node)))
                                                                         (log.debug "About to eval AJAX-received script " #\Newline script)
                                                                         ;; isolate the local bindings from the script to be executed
                                                                         ;; and only bind with the given name what we explicitly list here
                                                                         ((lambda (_script current-ajax-answer)
                                                                            (eval _script)) script response)
                                                                         (log.debug "Finished eval-ing AJAX-received script")))
                                                                     false true)))
            (log.debug "Calling script-evaluator...")
            (script-evaluator response args)
            (log.debug "...script-evaluator returned")))))

;;;;;;
;;; debug

;; from http://turtle.dojotoolkit.org/~david/recss.html
(defun wui.reload-css ()
  (dolist (link (document.getElementsByTagName "link"))
    (when (and (>= (.indexOf (.toLowerCase link.rel) "stylesheet") 0)
               link.href)
      (bind ((href (.replace link.href (regexp "(&|\\?)forceReload=\\d+") "")))
        (setf link.href (wui.append-query-parameter href "forceReload" (.valueOf (new Date))))))))

;;;;;;
;;; scroll

(defun wui.reset-scroll-position ((content :by-id))
  (when content
    (bind ((form (aref document.forms 0))
           (sx (aref form #.+scroll-x-parameter-name+))
           (sy (aref form #.+scroll-y-parameter-name+)))
      (log.debug "Restoring scroll position: " sx.value sy.value)
      (setf content.scrollLeft sx.value)
      (setf content.scrollTop sy.value))))

(defun wui.save-scroll-position ((content :by-id))
  (when content
    (bind ((form (aref document.forms 0))
           (sx (aref form #.+scroll-x-parameter-name+))
           (sy (aref form #.+scroll-y-parameter-name+)))
      (log.debug "Saving scroll position: " content.scrollLeft content.scrollTop)
      (setf sx.value content.scrollLeft)
      (setf sy.value content.scrollTop))))

;;;;;;
;;; highlight

(defun wui.highlight-mouse-enter-handler (event (element :by-id))
  (dojo.addClass element "highlighted")
  (let ((parent element.parent-node))
    (while (not (= parent document))
      (dojo.removeClass parent "highlighted")
      (setf parent parent.parent-node)))
  (dojo.stopEvent event))

(defun wui.highlight-mouse-leave-handler (event (element :by-id))
  (dojo.removeClass element "highlighted"))

;;;;;
;;; fields

;; TODO rename to wui.primitive.*?
(defun wui.field.setup-simple-checkbox (checkbox-id checked-tooltip unchecked-tooltip)
  (bind ((checkbox (dojo.byId checkbox-id))
         (hidden (dojo.byId (+ checkbox-id "_hidden"))))
    (log.debug "Setting up simple checkbox " checkbox ", using hidden input " hidden)
    (wui.connect checkbox "onchange"
                 (lambda (event)
                   (let ((enabled checkbox.checked))
                     (log.debug "Propagating checkbox.checked of " checkbox " to the hidden field " hidden " named " hidden.name)
                     (setf hidden.value (if enabled
                                            "true"
                                            "false"))
                     (setf checkbox.title
                           (if enabled
                               checked-tooltip
                               unchecked-tooltip)))))
    (setf checkbox.wui-set-checked (lambda (enabled)
                                     (if (= checkbox.checked enabled)
                                         (return false)
                                         (progn
                                           (setf checkbox.checked enabled)
                                           ;; we need to be in sync, so call onchange explicitly
                                           (checkbox.onchange)
                                           (return true)))))
    (setf checkbox.wui-is-checked (lambda ()
                                    (return checkbox.checked)))))

(defun wui.field.setup-custom-checkbox (link-id checked-image unchecked-image checked-tooltip unchecked-tooltip checked-class unchecked-class)
  (bind ((link (dojo.byId link-id))
         (hidden (dojo.byId (+ link-id "_hidden"))))
    (log.debug "Setting up custom checkbox " link ", using hidden input " hidden)
    (bind ((image (aref (.get-elements-by-tag-name link "img") 0))
           (enabled (not (= hidden.value "false"))))
;; TODO:      (assert image)
      (if (and checked-image
               unchecked-image)
          (setf image.src (if enabled
                              checked-image
                              unchecked-image)))
      (setf link.className (if enabled
                               checked-class
                               unchecked-class))
      (setf link.title (if enabled
                           checked-tooltip
                           unchecked-tooltip)))
    (setf link.wui-set-checked (lambda (enabled)
                                 (setf hidden.value (if enabled
                                                        "true"
                                                        "false"))
                                 (if (and checked-image
                                          unchecked-image)
                                     (setf image.src (if enabled
                                                         checked-image
                                                         unchecked-image)))
                                 (setf link.className (if enabled
                                                          checked-class
                                                          unchecked-class))
                                 (setf link.title (if enabled
                                                      checked-tooltip
                                                      unchecked-tooltip))))
    (setf link.wui-is-checked (lambda ()
                                (return (not (= hidden.value "false")))))
    (setf link.name hidden.name)        ; copy name of the form input
    (setf link.onclick (lambda (event)
                         (link.wui-set-checked (not (link.wui-is-checked)))))))

(defun wui.field.update-popup-menu-select-field ((node :by-id) (field :by-id) value class)
  (if class
      (setf node.className class)
      (setf node.innerHTML value))
  (setf field.value value))

(defun wui.field.update-use-in-filter ((field :by-id) value)
  ;; TODO disable, or make transparent the other controls, too
  (field.wui-set-checked value))

(defun wui.field._setup-filter-field (widget-id use-in-filter-id)
  ;; for now it's shared between a few fields...
  (on-load
   (bind ((widget (dijit.byId widget-id))
          (listener (lambda ()
                      (wui.field.update-use-in-filter use-in-filter-id (!= "" (.getValue this))))))
     (assert widget)
     ;; TODO why not wui.connect?
     (widget.connect widget "onKeyUp" listener)
     (widget.connect widget "onChange" listener))))

(defun wui.field.setup-string-filter (widget-id use-in-filter-id)
  (wui.field._setup-filter-field widget-id use-in-filter-id))

(defun wui.field.setup-number-filter (widget-id use-in-filter-id)
  (wui.field._setup-filter-field widget-id use-in-filter-id))

;;;;;;
;;; generic function

(defun wui.create-generic-function (name)
  (bind ((generic-function (create)))
    (setf generic-function.name name)
    (return generic-function)))

(defun wui.register-generic-function-method (generic-function dispatch-type fn)
  (setf (slot-value generic-function dispatch-type) fn))

(defun wui.apply-generic-function (generic-function dispatch-type args)
  (bind ((class-precedence-list (slot-value wui.component-class-precedence-lists dispatch-type)))
    (assert class-precedence-list)
    (dolist (class class-precedence-list)
      (bind ((fn (slot-value generic-function class)))
        (when fn
          (return (fn.apply undefined args)))))))

;;;;;;
;;; setup component

(setf wui.setup-component-generic-function (wui.create-generic-function))

(defun wui.register-component-setup (type fn)
  (wui.register-generic-function-method wui.setup-component-generic-function type fn))

(defun wui.setup-component (id type &rest args &key &allow-other-keys)
  (wui.apply-generic-function wui.setup-component-generic-function type (array id args)))

;;;;;;
;;; i18n

(setf wui.i18n.resources (create))

(defun wui.i18n.localize (name)
  (let ((value (aref wui.i18n.resources name)))
    (unless value
      (log.warn "Resource not found for key '" name "'")
      (setf value name))
    (return value)))

(defun wui.i18n.process-resources (resources)
  (log.debug "Received " resources.length " l10n resources")
  (do ((idx 0 (+ idx 2)))
      ((>= idx resources.length))
    (bind ((name (aref resources idx))
           (value (aref resources (1+ idx))))
      (setf (aref wui.i18n.resources name) value))))

;;;;;;
;;; Context sensitive help

(setf wui.help.popup-timeout 400)
(setf wui.help.timer nil)

(defun wui.help.decorate-url (url id)
  (if (or (= id "")
          (= id undefined))
      (return url)
      (return (wui.append-query-parameter url #.(escape-as-uri +context-sensitive-help-parameter-name+) id))))

(defun wui.help.make-mouseover-handler (url)
  (return
    (lambda (event)
      (clearTimeout wui.help.timer) ; safe to call with garbage
      (setf wui.help.timer
            (setTimeout (lambda ()
                          (bind ((decorated-url url)
                                 (node event.target)
                                 (help wui.help.tooltip))
                            (while (not (= node document))
                              (setf decorated-url (wui.help.decorate-url decorated-url node.id))
                              (setf node node.parent-node))
                            (when (or (= help nil)
                                      (and help.has-loaded
                                           (not (= help.href decorated-url))))
                              (wui.help.teardown)
                              (setf help (new dojox.widget.DynamicTooltip
                                              (create :connectId (array event.target)
                                                      :position (array "below" "right")
                                                      :href decorated-url)))
                              (setf wui.help.tooltip help)
                              (help.open event.target))))
                        wui.help.popup-timeout))
      (dojo.stopEvent event))))

(defun wui.help.setup (event url)
  (bind ((handles (array))
         (aborter (lambda (event)
                    (dojo.style document.body "cursor" "default")
                    (foreach 'dojo.disconnect handles)
                    (wui.help.teardown)
                    (dojo.stopEvent event))))
    (handles.push (wui.connect document "mouseover" (wui.help.make-mouseover-handler url)))
    (handles.push (wui.connect document "click" aborter))
    (handles.push (wui.connect document "keypress" (lambda (event)
                                                     (when (= event.charOrCode dojo.keys.ESCAPE)
                                                       (aborter event)))))
    (dojo.style document.body "cursor" "help")
    (dojo.stopEvent event)))

(defun wui.help.teardown ()
  (when wui.help.tooltip
    (wui.help.tooltip.destroy)
    (setf wui.help.tooltip nil)
    (when wui.help.timer
      (clearTimeout wui.help.timer)
      (setf wui.help.timer nil))))

;;;;;;
;;; Border

(defun wui.attach-border ((element :by-id) style-class element-name)
  (if element
      (bind ((parent-element element.parentNode)
             (next-sibling element.nextSibling)
             (table-element (create-dom-node "table")))
        (if (or (not style-class)
                (not element-name)
                (and (not (= element-name true))
                     (not (= element-name element.tagName))))
            (return element))
        (if (dojo.isArray style-class)
            (dolist (one-class style-class)
              (dojo.addClass table-element one-class))
            (dojo.addClass table-element style-class))
        (progn
          ;; KLUDGE: we copy the id on the border not to confuse ajax what should be replaced
          (setf table-element.id element.id)
          (setf element.id ""))
        (flet ((create-dummy-div ()
                 (bind ((result (document.createElement "div")))
                   (dojo.addClass result "decoration")
                   (return result)))
               (create-row (row-kind)
                 (with-dom-nodes ((row-kind-element (+ "t" row-kind))
                                  (row-element "tr")
                                  (left-cell-element "td" :class "border-left")
                                  (cell-element "td" :class "border-center")
                                  (right-cell-element "td" :class "border-right"))
                   (when (= row-kind "head")
                     (dojo.place (create-dummy-div) left-cell-element "only")
                     (dojo.place (create-dummy-div) right-cell-element "only"))
                   (when (= row-kind "body")
                     (dojo.place (create-dummy-div) left-cell-element "only")
                     (dojo.place (create-dummy-div) right-cell-element "only"))
                   (dojo.place row-kind-element table-element)
                   (dojo.place row-element row-kind-element)
                   (dojo.place left-cell-element row-element)
                   (dojo.place cell-element row-element)
                   (dojo.place right-cell-element row-element)
                   (return cell-element))))
          (create-row "head")
          (bind ((cell-element (create-row "body")))
            (create-row "foot")
            (dojo.place element cell-element)
            (if next-sibling
                (dojo.place table-element next-sibling "before")
                (dojo.place table-element parent-element)))))
      (log.warn "Cannot attach border to element")))

;;;;;;
;;; End of story

(log.debug "Finished evaluating wui.js")
