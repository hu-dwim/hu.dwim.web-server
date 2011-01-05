;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Variables

(def (special-variable e) *dojo-skin-name*)

(def (special-variable e) *dojo-file-name*)

(def (special-variable e) *dojo-directory-name*)

;;;;;;
;;; application-with-dojo-support

(def (class* ea) application-with-dojo-support (application)
  ((dojo-skin-name "tundra")
   (dojo-file-name "dojo.js")
   (dojo-directory-name (or (find-latest-dojo-directory-name (system-relative-pathname :hu.dwim.web-server "www/libraries/") :otherwise :warn)
                            "dojotoolkit/"))))

(def method startup-broker :after ((self application-with-dojo-support))
  (unless (dojo-directory-name-of self)
    (warn "The DOJO-DIRECTORY-NAME slot of application ~A is not initialized by the time the server was started! Please refer to the install guide (e.g. on http://dwim.hu) or the sources for details on how to build dojo." self)))

(def method call-in-application-environment :around ((application application-with-dojo-support) session thunk)
  (bind ((*dojo-skin-name* (or (dojo-skin-name-of application)
                               *dojo-skin-name*))
         (*dojo-file-name* (or (dojo-file-name-of application)
                               *dojo-file-name*))
         (*dojo-directory-name* (or (dojo-directory-name-of application)
                                    *dojo-directory-name*)))
    (call-next-method)))

;;;;;;
;;; Rendering

(def special-variable *dojo-widget-ids*)

(macrolet ((x (&body entries)
             `(progn
                ,@(iter (for (name dojo-name) :on entries :by #'cddr)
                        (collect `(def (constant e) ,name ,dojo-name))))))
  (x
   +dijit/accordion-container+   "dijit.layout.AccordionContainer"
   +dijit/border-container+      "dijit.layout.BorderContainer"
   +dijit/content-pane+          "dijit.layout.ContentPane"
   +dijit/split-container+       "dijit.layout.SplitContainer"
   +dijit/tab-container+         "dijit.layout.TabContainer"
   +dijit/button+                "dijit.form.Button"
   +dijit/drop-down-button+      "dijit.form.DropDownButton"
   +dijit/date-text-box+         "dijit.form.DateTextBox"
   +dijit/simple-text-area+      "dijit.form.SimpleTextarea"
   +dijit/text-box+              "dijit.form.TextBox"
   +dijit/time-text-box+         "dijit.form.TimeTextBox"
   +dijit/combo-box+             "dijit.form.ComboBox"
   +dijit/filtering-select+      "dijit.form.FilteringSelect"
   +dijit/number-text-box+       "dijit.form.NumberTextBox"
   +dijit/dialog+                "dijit.Dialog"
   +dijit/tooltip-dialog+        "dijit.TooltipDialog"
   +dijit/editor+                "dijit.Editor"
   +dijit/menu+                  "dijit.Menu"
   +dijit/menu-bar+              "dijit.MenuBar"
   +dijit/menu-item+             "dijit.MenuItem"
   +dijit/menu-bar-item+         "dijit.MenuBarItem"
   +dijit/menu-separator+        "dijit.MenuSeparator"
   +dijit/popup-menu-item+       "dijit.PopupMenuItem"
   +dijit/popup-menu-bar-item+   "dijit.PopupMenuBarItem"
   +dijit/inline-edit-box+       "dijit.InlineEditBox"
   ))

(def (function e) find-latest-dojo-directory-name (directory &key (otherwise :cerror))
  (loop
    (with-simple-restart (retry "Try searching for dojo directories again in ~A" directory)
      (bind ((dojo-dir (first (sort (remove-if [not (starts-with-subseq "dojo" !1)]
                                               (mapcar [last-elt (pathname-directory !1)]
                                                       (cl-fad:list-directory directory)))
                                    #'string>=))))
      (return
        (if dojo-dir
            (string+ dojo-dir "/")
            (handle-otherwise/value otherwise :default-message (list "Seems like there's not any dojo directory in ~S. Hint: see hu.dwim.web-server/etc/build-dojo.sh" directory))))))))

(pushnew 'dojo-widget-collector/wrapper *xhtml-body-environment-wrappers*)

(def function dojo-widget-collector/wrapper (thunk)
  (bind ((*dojo-widget-ids* '()))
    (multiple-value-prog1
        (funcall thunk)
      (when *dojo-widget-ids*
        ;; NOTE: instantiation must happend before any other js code tinkers with the dojo widgets,
        ;; therefore we wrap here again with the js script collapser to emit us before the parent
        ;; WITH-XHTML-BODY-ENVIRONMENT emits the rest of the js stuff.
        `xml,@(with-xhtml-body-environment (:wrappers '(js-script-collapser/wrapper))
                `js(on-load
                    ;; using `js-onload here is a no go (ordering issue)
                    (wui.io.instantiate-dojo-widgets (array ,@*dojo-widget-ids*))))))))

(def macro render-dojo-widget ((&optional (id '(generate-unique-string/frame "_w")))
                                &body body)
  (once-only (id)
    `(progn
       ,@body
       (push ,id *dojo-widget-ids*)
       (values))))

(def (macro e) render-dojo-dialog ((id-var &key title (width "400px") (attach-point '`js-piece(slot-value document.body ,(generate-unique-string/frame))))
                                    &body body)
  (once-only (width attach-point)
    `(bind ((,id-var (generate-unique-string/frame)))
       `js(bind ((dialog (or ,,attach-point
                             (new dijit.Dialog (create :id ,,id-var
                                                       :title ,,title
                                                       :content ,(transform-xml/js-quoted ,@body))))))
            (setf ,,attach-point dialog)
            ;; TODO qq should work with ,,(when ...)
            (bind ((width ,,width))
              (when width
                (dojo.style dialog.domNode "width" width)))
            (dialog.show)))))

(def (macro e) render-dojo-dialog/buttons (&body entires)
  `<table (:style "float: right;")
     <tr ,@,@(iter (for (label js) :in entires)
                   (collect `<td ,(render-dojo-button ,label :on-click ,js)>))>>)

(def (macro e) render-dojo-button (label &key on-click)
  `<div (:dojoType   #.+dijit/button+
         :label      ,,label
         :onClick    ,,on-click)>)

#+nil ;; TODO this should work instead of the above
(def (macro e) render-dojo-button (label &key on-click)
  `(render-dojo-widget* (+dijit/button+ :label ,label :on-click ,on-click)))

(def (macro e) render-dojo-widget* ((dojo-type &rest attributes &key id (xml-element-name "span")
                                               &allow-other-keys)
                                     &body body)
  (remove-from-plistf attributes :id :xml-element-name)
  `<,,xml-element-name (:id ,,id
                        :dojoType ,,dojo-type
                        ,@,@(iter (for (name value) :on attributes :by #'cddr)
                                  (collect (make-xml-attribute (hyphened-to-camel-case (string-downcase name)) (make-xml-unquote value)))))
                       ,@,@body>)
