;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Logging

(macrolet ((forward (name &optional only-in-debug)
             `(def (js-macro e) ,(symbolicate "log." name) (&rest args)
                (when ,(if only-in-debug
                           '*debug-client-side*
                           t)
                 (list* ',(symbolicate "window.console." name) args)))))
  (forward |debug| t)
  (forward |info| t)
  (forward |warn|)
  (forward |error|)
  (forward |critical|))

;;;;;;
;;; Lift some definitions to JavaScript

(def (js-macro e) |in-package| (package)
  (declare (ignore package))
  (values))

(def (js-macro e) |bind| (args &body body)
  {with-preserved-readtable-case
   `(let ,ARGS ,@BODY)})

(def (js-macro e) |on-load| (&body body)
  {with-preserved-readtable-case
   `(dojo.addOnLoad
     (lambda ()
       ,@BODY))})

(def (js-macro e) |with-dom-nodes| (bindings &body body)
  (iter (for binding :in bindings)
        (bind (((variable-name tag-name &key ((:|class| class))) binding))
          (collect {with-preserved-readtable-case
                     `(,VARIABLE-NAME (document.createElement ,TAG-NAME))} :into node-bindings)
          (when class
            (collect {with-preserved-readtable-case
                       `(dojo.addClass ,VARIABLE-NAME ,CLASS)} :into forms)))
        (finally (return {with-preserved-readtable-case
                           `(bind (,@NODE-BINDINGS)
                              ,@FORMS
                              ,@BODY)}))))

(def (js-macro e) |create-dom-node| (name)
  {with-preserved-readtable-case
    `(document.createElement ,NAME)})

(def (js-macro e) |hide-dom-node| (node)
  {with-preserved-readtable-case
   `(setf (slot-value (slot-value ,NODE 'style) 'display) "none")})

(def (js-macro e) |show-dom-node| (node)
  {with-preserved-readtable-case
   `(setf (slot-value (slot-value ,NODE 'style) 'display) "")})

(def (js-macro e) |get-first-child-with-tag-name| (node tag-name)
  {with-preserved-readtable-case
   `(aref (.get-elements-by-tag-name ,NODE ,TAG-NAME) 0)})

(def (js-macro e) |when-bind| (var condition &body body)
  {with-preserved-readtable-case
   `(let ((,VAR ,CONDITION))
      (if ,VAR
          (progn
            ,@BODY)))})

(def (js-macro e) |awhen| (condition &body body)
  {with-preserved-readtable-case
    `(when-bind it ,CONDITION
       ,@BODY)})

(def (js-macro e) |aif| (condition then else)
  {with-preserved-readtable-case
   `(let ((it ,CONDITION))
      (if it
          ,THEN
          ,ELSE))})

(def (js-macro e) $ (thing)
  {with-preserved-readtable-case
    `(dojo.byId ,THING)})

;; TODO this should probably be exported, but that causes package headaches...
(def js-macro |defun| (name args &body body)
  (bind ((name-pieces (cl-ppcre:split "\\." (symbol-name name)))
         (arg-names (iter (for arg :in args)
                          (collect (if (listp arg)
                                       (car arg)
                                       arg))))
         (arg-processors (iter (for arg :in args)
                               (when (listp arg)
                                 (unless (= (length arg) 2)
                                   (error "Hm, what do you mean by ~S?" arg))
                                 (let ((name (first arg)))
                                   (case (second arg)
                                     (:|by-id|
                                      (collect `{with-preserved-readtable-case
                                                 (setf ,NAME ($ ,NAME))}))
                                     (:|widget-by-id|
                                      (collect `{with-preserved-readtable-case
                                                 (setf ,NAME (dojo.widget.by-id ,NAME))}))
                                     (t
                                      (collect `{with-preserved-readtable-case
                                                 (when (=== ,NAME undefined)
                                                   (setf ,NAME ,(CADR ARG)))}))))))))
    (if (length= 1 name-pieces)
        {with-preserved-readtable-case
         `(HU.DWIM.QUASI-QUOTE.JS:defun ,NAME ,ARG-NAMES
            ,@ARG-PROCESSORS
            ,@BODY)}
        {with-preserved-readtable-case
         `(progn
            (setf ,NAME (lambda ,ARG-NAMES
                          ,@ARG-PROCESSORS
                          ,@BODY)))})))

(def (js-macro e) |with-ajax-answer-logic| (data &body body)
  (with-unique-js-names (result-node)
    {with-preserved-readtable-case
      `(progn
         (log.info "Processing AJAX answer " ,DATA)
         (setf ,DATA (get-first-child-with-tag-name ,DATA "ajax-response"))
         (unless ,DATA
           (log.warn "AJAX ajax-response node is nil, probably a malformed response, maybe a full page load due to an unregistered action id?")
           (throw (new hdws.communication-error "AJAX answer is empty")))
         (log.debug "Found ajax-response DOM node")
         (let ((,RESULT-NODE (get-first-child-with-tag-name ,DATA "result")))
           (if (or (not ,RESULT-NODE)
                   (not (= (dojo.string.trim (dojox.xml.parser.textContent ,RESULT-NODE))
                           "success")))
               (let ((error-message (hdws.i18n.localize "unknown-server-error")))
                 (when-bind error-node (get-first-child-with-tag-name ,DATA "error-message")
                   (setf error-message (dojox.xml.parser.textContent error-node)))
                 (when error-message
                   (alert error-message)))
               (progn
                 ,@BODY))))}))

(def (js-macro e) |assert| (expression &rest args-to-throw)
  (unless args-to-throw
    (setf args-to-throw (list (string+ "Assertion failed: " (princ-to-string expression)))))
  {with-preserved-readtable-case
    `(unless ,EXPRESSION
       ,(IF *DEBUG-CLIENT-SIDE*
            `(bind ((to-be-thrown (array ,@ARGS-TO-THROW)))
               (log.error "Assertion failed, will throw " to-be-thrown)
               (when dojo.config.isDebug
                 debugger)
               (throw to-be-thrown))
            `(throw (array ,@ARGS-TO-THROW))))})
