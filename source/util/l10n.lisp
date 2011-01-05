;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Listener definer

(def (definer e) localization-loader-callback (name asdf-system base-directory &key (log-discriminator (string-downcase asdf-system)) (setup-readtable-function ''setup-readtable))
  (with-standard-definer-options name
    (once-only (setup-readtable-function asdf-system base-directory log-discriminator)
      `(progn
         (def function ,name (locale-name)
           (l10n.debug "Loading ~A localizations for locale ~S" ,log-discriminator locale-name)
           (bind ((file (merge-pathnames (string+ locale-name ".lisp") (system-relative-pathname ,asdf-system ,base-directory))))
             (when (probe-file file)
               (bind ((*readtable* (copy-readtable nil)))
                 (awhen ,setup-readtable-function
                   (funcall it))
                 (cl-l10n::load-resource-file file)
                 (l10n.info "Loaded ~A localizations for locale ~S from ~A" ,log-discriminator locale-name file)))))
         (register-locale-loaded-listener ',name)))))

(def localization-loader-callback localization-loader/hu.dwim.web-server :hu.dwim.web-server "localization/")

;;;;;;
;;; Localized string reader

(def (constant e) +missing-localization-style-class+ (coerce "missing-localization" 'simple-base-string))

(def function localized-string-reader (stream c1 c2)
  (declare (ignore c2))
  (unread-char c1 stream)
  (bind ((key (read stream))
         (capitalize? (to-boolean (and (> (length key) 0)
                                       (upper-case-p (elt key 0))))))
    (if (ends-with-subseq "<>" key)
        `(%localized-string-reader/impl<> ,(string-downcase key) ,capitalize?)
        `(%localized-string-reader/impl ,(string-downcase key) ,capitalize?))))

(def function %localized-string-reader/impl<> (key capitalize?)
  (bind (((:values str found?) (lookup-resource (subseq key 0 (- (length key) 2)))))
    (when capitalize?
      (setf str (capitalize-first-letter str)))
    (if found?
        `xml,str
        <span (:class #.+missing-localization-style-class+)
          ,str>)))

(def function %localized-string-reader/impl (key capitalize?)
  (bind (((:values str found?) (lookup-resource key)))
    (when (and capitalize?
               found?)
      (setf str (capitalize-first-letter str)))
    str))

;;;;;;
;;; Localization primitives

(def (function e) fully-qualified-symbol-name/for-localization-entry (symbol)
  ;; make the separator independent of the exported state of the symbol
  (string-downcase (fully-qualified-symbol-name symbol :separator ":")))

(def (function e) localized-mime-type-description (mime-type)
  (lookup-first-matching-resource
    ("mime-type" mime-type)))

(def (function e) localized-mime-type-description<> (mime-type)
  (bind (((:values str found) (localized-mime-type-description mime-type)))
    <span (:class ,(string+ "slot-name " (unless found +missing-localization-style-class+)))
      ,str>))

(def function localized-boolean-value (value)
  (bind (((:values str found?) (localize (if value "boolean-value.true" "boolean-value.false"))))
    (values str found?)))

(def function localized-boolean-value<> (value)
  (bind (((:values str found?) (localized-boolean-value value)))
    <span (:class ,(string+ "boolean " (unless found? +missing-localization-style-class+)))
      ,str>))

;;;;;;
;;; Utilities

;; TODO
#+nil
(def (function e) render-client-timezone-offset-probe ()
  "Renders an input field with a callback that will set the CLIENT-TIMEZONE slot of the session when the form is submitted."
  (with-unique-js-names (id)
    <input (:id ,id
            :type "hidden"
            :name (client-state-sink (value)
                    (bind ((offset (parse-integer value :junk-allowed #t)))
                      (if offset
                          (bind ((local-time (parse-rfc3339-timestring value)))
                            (l10n.debug "Setting client timezone from ~A" local-time)
                            (setf (client-timezone-of (context.session *context*)) (timezone-of local-time)))
                          (progn
                            (app.warn "Unable to parse the client timezone offset: ~S" value)
                            (setf (client-timezone-of (context.session *context*)) +utc-zone+)))))) >
    `js-onload(setf (slot-value ($ ,id) 'value) (dojo.date.stamp.toISOString (new *date)))))

(def special-variable *fallback-locale-for-functional-localizations* "en"
  "Used as a fallback locale if a functional localization can not be found and there's no *application* that would provide a default locale. It's not possible to use the usual name fallback strategy for functional localizations, so make sure that the default locale has a 100% coverage for them, otherwise it may effect the behavior of the application in certain situations.")

(def function funcall-localization-function (name &rest args)
  (apply-localization-function name args))

(def function apply-localization-function (name &optional args)
  (lookup-resource name :arguments args
                   :otherwise (lambda ()
                                (with-locale (locale *fallback-locale-for-functional-localizations*)
                                  (lookup-resource name :arguments args
                                                   :otherwise [error "Functional localization ~S is missing even for the fallback locale ~A" name (current-locale)])))))
