;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (constant e) +js-i18n-broker/default-path+ "/hdws/js/locale-specific.js")

(def definer js-localization (locale &body resources)
  `(progn
     (def localization ,locale ,@resources)
     (%register-js-resource-names '(,@(mapcar 'first resources)))))

(def function %register-js-resource-names (names)
  (dolist (name names)
    (setf (js-i18n-resource? name) t)))

;; "Contains the name of resources (either symbols or strings) that should to be sent down to the client."
(def (namespace :test 'equal
                :finder-name    %js-i18n-resource?)
  js-i18n-resource-registry)

(def special-variable *js-i18n-resource-registry/last-modified-at* (local-time:now))

(def (function i) js-i18n-resource? (name &rest args &key otherwise)
  (declare (ignore otherwise) (dynamic-extent args))
  (apply '%js-i18n-resource? name args))

(def function (setf js-i18n-resource?) (value name)
  (prog1
      (setf (%js-i18n-resource? name) value)
    (setf *js-i18n-resource-registry/last-modified-at* (local-time:now))))

(def (namespace :test 'equal
                :finder-name js-i18n-response-cache-entry
                :collector-name nil
                :iterator-name nil)
  js-i18n-response-cache)

(def class* js-i18n-broker (broker-at-path)
  ()
  (:default-initargs :path +js-i18n-broker/default-path+))

(def method produce-response ((self js-i18n-broker) request)
  (bind ((cache-key (mapcar 'locale-name (current-locale)))
         (cache-entry (js-i18n-response-cache-entry cache-key :otherwise nil))
         (generated-at (first cache-entry))
         (response-body (second cache-entry)))
    (when (or (not response-body)
              (local-time:timestamp< generated-at *js-i18n-resource-registry/last-modified-at*))
      (setf response-body (emit-into-js-stream-buffer (:external-format :utf-8)
                            (serve-js-i18n-response)))
      (setf (js-i18n-response-cache-entry cache-key) (list (local-time:now) response-body)))
    (make-byte-vector-response* response-body
                                :last-modified-at *js-i18n-resource-registry/last-modified-at*
                                :seconds-until-expires (* 60 60)
                                :content-type (content-type-for +javascript-mime-type+ :utf-8))))

(def function serve-js-i18n-response ()
  `js(hdws.i18n.process-resources
      (array ,@(bind ((entries ()))
                 (do-namespace (js-i18n-resource-registry name)
                   (flet ((stringify (value)
                            (etypecase value
                              (symbol (string-downcase value))
                              (string value))))
                     (push (stringify (localize name)) entries)
                     (push (stringify name) entries)))
                 entries))))
