;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

;; status of this code: completely ad-hoc random experiment

(def special-variable *dojo-application* (make-instance 'application-with-dojo-support :path "dojo"))

(def entry-point (*dojo-application* :path "xhr.data")
  (hu.dwim.web-server::make-functional-response/ajax-aware-client ()
    <dom-replacements (:xmlns #.+xml-namespace-uri/xhtml+)
      <div (:id "x42"
            :class "foo"
            :dojoType "dojjtyppe")>>))

;; this is a stripped down version of the one in hu.dwim.presentation/source/component/widget/frame.lisp
(def with-macro* emit-html-document/dojo (&key stylesheet-uris (script-uris (list (hu.dwim.uri:parse-uri "/hdws/js/main.dojo.js")))
                                               (content-mime-type +xhtml-mime-type+) (encoding :utf-8) title
                                               (parse-dojo-widgets-on-load #f) (dojo-file-name "dojo.js") (dojo-skin-name "tundra")
                                               (dojo-release-uri (hu.dwim.uri:parse-uri (string+ "/static/hdws/libraries/" *dojo-directory-name*))))
  (bind ((application *application*)
         (application-path (path-of application))
         (debug-client-side? (debug-client-side? nil)))
    (emit-xhtml-prologue encoding +xhtml-1.1-doctype+)
    <html (:xmlns     #.+xml-namespace-uri/xhtml+
           xmlns:dojo #.+xml-namespace-uri/dojo+)
      <head
        <meta (:http-equiv +header/content-type+
               :content ,(content-type-for content-mime-type encoding))>
        <title ,title>
        ,(dolist (file '("dojo/resources/dojo.css"
                         "dijit/themes/dijit.css"
                         "dijit/themes/dijit_rtl.css"))
            <link (:rel "stylesheet"
                   :type "text/css"
                   :href ,(string+ "/static/hdws/libraries/" *dojo-directory-name* file))>)
        ,(foreach (lambda (stylesheet-uri)
                    <link (:rel "stylesheet"
                           :type "text/css"
                           :href ,(bind ((uri (hu.dwim.uri:clone-uri stylesheet-uri)))
                                    (hu.dwim.uri:prepend-path uri application-path)
                                    (hu.dwim.uri:print-uri-to-string uri)))>)
                  stylesheet-uris)
        <script (:type +javascript-mime-type+)
          ,(string+ "djConfig = { baseUrl: '" (hu.dwim.uri:print-uri-to-string dojo-release-uri) "dojo/'"
                    ", parseOnLoad: " (to-js-boolean parse-dojo-widgets-on-load)
                    ", isDebug: " (to-js-boolean debug-client-side?)
                    ;; TODO add separate flag for debugAtAllCosts
                    ", debugAtAllCosts: " (to-js-boolean debug-client-side?)
                    ;; TODO locale should come from either the session or from frame/widget
                    ", locale: " (to-js-literal (locale-name (locale (first (ensure-list (default-locale-of application))))))
                    "}")>
        <script (:type +javascript-mime-type+
                 :src  ,(bind ((uri (hu.dwim.uri:clone-uri dojo-release-uri)))
                          (hu.dwim.uri:append-path uri "dojo/")
                          (hu.dwim.uri:append-path uri (if debug-client-side?
                                                           (string+ dojo-file-name ".uncompressed.js")
                                                           dojo-file-name))
                          (hu.dwim.uri:print-uri-to-string uri)))
                 ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                 "">
        ,(foreach (lambda (script-uri)
                    <script (:type +javascript-mime-type+
                             :src  ,(bind ((uri (hu.dwim.uri:clone-uri script-uri)))
                                      (unless (starts-with #\/ (hu.dwim.uri:path-of uri))
                                        (hu.dwim.uri:prepend-path uri application-path))
                                      (when debug-client-side?
                                        (setf (hu.dwim.uri:query-parameter-value uri "debug") "t"))
                                      (hu.dwim.uri:print-uri-to-string uri)))
                      ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                      "">)
                  script-uris)>
      <body (:class ,dojo-skin-name)
        <form (:method "post"
               :enctype ,+form-encoding/multipart-form-data+)
          ,@(with-xhtml-body-environment ()
              (-with-macro/body-))>>>))

(def entry-point (*dojo-application* :path "simple-input")
  (flet ((body ()
           (make-buffered-functional-html-response ()
             (emit-html-document/dojo ()
               <input (:id "input42"
                       :data-dojo-props "type:'text', value:54, required:true")>
               `js(on-load
                   (dojo.require "dijit.form.NumberTextBox"))
               <a (:href "#"
                   :onclick `js-inline(bind ((node ($ "input42")))
                                        (dojo.parser.instantiate (array (create :type "dijit.form.NumberTextBox"
                                                                                :node node
                                                                                :fastpath true)))))
                  "click me to instantiate dojo widgets">
               (render-dojo-widget ("dijit.form.NumberTextBox")
                 <div (:id ,-id-)>)))))
    (call-in-application-environment *application* nil #'body)))
