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

(def localization-loader-callback wui-localization-loader :hu.dwim.web-server "localization/")

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

(def function fully-qualified-symbol-name/for-localization-entry (symbol)
  ;; make the separator independent of the exported state of the symbol
  (string-downcase (fully-qualified-symbol-name symbol :separator ":")))

(def (function e) localized-mime-type-description (mime-type)
  (lookup-first-matching-resource
    ("mime-type" mime-type)))

(def (function e) localized-mime-type-description<> (mime-type)
  (bind (((:values str found) (localized-mime-type-description mime-type)))
    <span (:class ,(string+ "slot-name " (unless found +missing-localization-style-class+)))
      ,str>))

(def method localize ((class class))
  (lookup-first-matching-resource
    ("class-name" (fully-qualified-symbol-name/for-localization-entry (class-name class)))
    ("class-name" (string-downcase (class-name class)))))

(def (generic e) localized-slot-name (slot &key capitalize-first-letter prefix-with)
  (:method ((slot effective-slot-definition) &key &allow-other-keys)
    (bind ((slot-name (slot-definition-name slot)))
      (lookup-first-matching-resource
        ((class-name (owner-class-of-effective-slot-definition slot)) slot-name)
        ("slot-name" slot-name)
        ("class-name" (awhen (find-class-for-type (slot-definition-type slot))
                        (class-name it))))))

  (:method :around ((slot effective-slot-definition) &key (capitalize-first-letter #t) prefix-with)
    (bind (((:values str found?) (call-next-method)))
      (assert str)
      (when capitalize-first-letter
        (setf str (capitalize-first-letter str)))
      (values (if prefix-with
                  (string+ prefix-with str)
                  str)
              found?))))

(def function localized-slot-name<> (slot &rest args)
  (bind (((:values str found) (apply #'localized-slot-name slot args)))
    <span (:class ,(string+ "slot-name " (unless found +missing-localization-style-class+)
                            ;; TODO this is fragile here, should use a public api in dmm
                            ;; something like associationp
                            ;;(when (typep slot 'hu.dwim.meta-model:effective-association-end)
                            ;;  "association")
                            ))
      ,str>))

;; TODO: what is special about this function to classes? could be easily generalized to functions, etc.
(def function localized-class-name (class &key capitalize-first-letter with-article plural)
  (assert (typep class 'class))
  (bind (((:values class-name found?) (localize class)))
    (when plural
      (setf class-name (plural-of class-name)))
    (when with-article
      (setf class-name (if plural
                           (with-definite-article class-name)
                           (with-indefinite-article class-name))))
    (values (if capitalize-first-letter
                (capitalize-first-letter class-name)
                class-name)
            found?)))

;; TODO: what is special about this function to classes? could be easily generalized to functions, etc.
(def function localized-class-name<> (class &key capitalize-first-letter with-indefinite-article)
  (assert (typep class 'class))
  (bind (((:values class-name found?) (localize class)))
    (when with-indefinite-article
      (bind ((article (with-indefinite-article class-name)))
        (write (if capitalize-first-letter
                   (capitalize-first-letter article)
                   article)
               :stream *xml-stream*)
        (write-char #\Space *xml-stream*)))
    <span (:class ,(string+ "class-name " (unless found? +missing-localization-style-class+)))
          ,(if (and capitalize-first-letter
                    (not with-indefinite-article))
               (capitalize-first-letter class-name)
               class-name)>))

(def function localized-boolean-value (value)
  (bind (((:values str found?) (localize (if value "boolean-value.true" "boolean-value.false"))))
    (values str found?)))

(def function localized-boolean-value<> (value)
  (bind (((:values str found?) (localized-boolean-value value)))
    <span (:class ,(string+ "boolean " (unless found? +missing-localization-style-class+)))
      ,str>))

(def function member-value-name/for-localization-entry (value)
  (typecase value
    (symbol (string-downcase value))
    (class (string-downcase (class-name value)))
    (integer (integer-to-string value))
    (number (princ-to-string value))
    (t nil)))

(def function localized-enumeration-member (member-value &key class slot capitalize-first-letter)
  ;; TODO fails with 'person 'sex: '(OR NULL SEX-TYPE)
  (unless class
    (setf class (when slot
                  (owner-class-of-effective-slot-definition slot))))
  (bind ((member-value-name (member-value-name/for-localization-entry member-value))
         (slot-definition-type (when slot
                                 (slot-definition-type slot)))
         (class-name (when class
                       (class-name class)))
         (slot-name (when slot
                      (symbol-name (slot-definition-name slot))))
         ((:values str found?)
          (lookup-first-matching-resource
            (when (and class-name
                       slot-name)
              class-name slot-name member-value-name)
            (when slot-name
              slot-name member-value-name)
            (when (and slot-definition-type
                       ;; TODO strip off (or null ...) from the type
                       (symbolp slot-definition-type))
              slot-definition-type member-value-name)
            ("member-type-value" member-value-name))))
    (when (and (not found?)
               (typep member-value 'class))
      (setf (values str found?) (localized-class-name member-value)))
    (when capitalize-first-letter
      (setf str (capitalize-first-letter str)))
    (values str found?)))

(def function localized-enumeration-member<> (member-value &key class slot capitalize-first-letter)
  (bind (((:values str found?) (localized-enumeration-member member-value :class class :slot slot
                                                             :capitalize-first-letter capitalize-first-letter)))
    <span (:class ,(unless found? +missing-localization-style-class+))
      ,str>))

(def constant +timestamp-format+ '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
                                   (:hour 2) #\: (:min 2) #\: (:sec 2)))

(def (function e) localized-timestamp (timestamp &key verbosity (relative-mode #f) display-day-of-week)
  (declare (ignore verbosity relative-mode display-day-of-week))
  ;; TODO many
  (local-time:format-timestring nil timestamp :format +timestamp-format+)
  ;; TODO (cl-l10n:format-timestamp nil timestamp)
  )

#+nil ;; TODO port this old localized-timestamp that supports relative mode
(def function localized-timestamp (local-time &key
                                              (relative-mode nil)
                                              (display-day-of-week nil display-day-of-week-provided?))
  "DWIMish timestamp printer. RELATIVE-MODE requests printing human friendly dates based on (NOW)."
  (setf local-time (local-time:adjust-local-time local-time (set :timezone *client-timezone*)))
  (bind ((*print-pretty* nil)
         (*print-circle* nil)
         (cl-l10n::*time-zone* nil)
         (now (local-time:now)))
    (with-output-to-string (stream)
      (with-decoded-local-time (:year year1 :month month1 :day day1 :day-of-week day-of-week1) now
        (with-decoded-local-time (:year year2 :month month2 :day day2) local-time
          (bind ((year-distance (abs (- year1 year2)))
                 (month-distance (abs (- month1 month2)))
                 (day-distance (- day2 day1))
                 (day-of-week-already-encoded? nil)
                 (day-of-week-should-be-encoded? (or display-day-of-week
                                                     (and relative-mode
                                                          (zerop year-distance)
                                                          (zerop month-distance)
                                                          (< (abs day-distance) 7))))
                 (format-string/date (if (and relative-mode
                                              (zerop year-distance))
                                         (if (zerop month-distance)
                                             (cond
                                               ((<= -1 day-distance 1)
                                                (setf day-of-week-already-encoded? t)
                                                (setf day-of-week-should-be-encoded? nil)
                                                (case day-distance
                                                  (0  (lookup-resource "today"))
                                                  (-1 (lookup-resource "yesterday"))
                                                  (1  (lookup-resource "tomorrow"))))
                                               (t
                                                ;; if we are within the current week
                                                ;; then print only the name of the day.
                                                (if (and (< (abs day-distance) 7)
                                                         (<= 1 ; but sunday can be a potential source of confusion
                                                             (+ day-of-week1
                                                                day-distance)
                                                             6))
                                                    (progn
                                                      (setf day-of-week-already-encoded? t)
                                                      "%A")
                                                    (progn
                                                      (setf day-of-week-should-be-encoded? nil)
                                                      "%b. %e."))))
                                             "%b. %e.")
                                         "%Y. %b. %e."))
                 (format-string (string+ format-string/date
                                         (if (or day-of-week-already-encoded?
                                                 (not day-of-week-should-be-encoded?)
                                                 (and display-day-of-week-provided?
                                                      (not display-day-of-week)))
                                             ""
                                             " %A")
                                         " %H:%M:%S")))
            (cl-l10n::print-time-string format-string stream
                                        (local-time:universal-time
                                         (local-time:adjust-local-time! local-time
                                           (set :timezone local-time:*default-timezone*)))
                                        (current-locale))))))))

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

;; TODO: resolve this duality among localize and localized-instance-name, why do we have both?
;; TODO localized-instance-name is quite a bad name, because it's not the instance's name, only a mere human readable, short text representation
(def (generic e) localized-instance-name (value)
  (:method (value)
    (bind ((*print-level* 3)
           (*print-length* 3))
      (princ-to-string value)))

  (:method ((value number))
    value)

  (:method ((value string))
    value)

  (:method ((value sequence))
    (cond ((emptyp value)
           (lookup-resource "sequence.empty"))
          ((proper-list-p value)
           (bind ((length (length value))
                  (first (elt value 0))
                  (class (class-of first))
                  (elements-name (lookup-resource "sequence.element")))
             (string+ (princ-to-string length)
                      " "
                      (when (every (of-type class) value)
                        (localized-class-name class))
                      " "
                      (if (= length 1)
                          elements-name
                          (plural-of elements-name))
                      " "
                      (call-next-method))))
          (t
           (call-next-method))))

  (:method ((class class))
    (localized-class-name class))

  (:method ((timestamp local-time:timestamp))
    (localized-timestamp timestamp))

  (:method ((function function))
    (bind ((name (function-name function)))
      (cond ((symbolp name)
             (lookup-first-matching-resource
               ("function-name" (fully-qualified-symbol-name/for-localization-entry name))
               ("function-name" (string-downcase name))))
            ((and (consp name)
                  (eq (first name) 'macro-function))
             (lookup-first-matching-resource
               ("macro-name" (fully-qualified-symbol-name/for-localization-entry (second name)))
               ("macro-name" (string-downcase (second name)))))
            (t "unknown function")))))

(def function funcall-localization-function (name &rest args)
  (apply-localization-function name args))

;;;;;
;;; Initialized to "en" in l10n.lisp

(def special-variable *fallback-locale-for-functional-localizations* "en"
  "Used as a fallback locale if a functional localization can not be found and there's no *application* that would provide a default locale. It's not possible to use the usual name fallback strategy for functional localizations, so make sure that the default locale has a 100% coverage for them, otherwise it may effect the behavior of the application in certain situations.")

(def function apply-localization-function (name &optional args)
  (lookup-resource name :arguments args
                   :otherwise (lambda ()
                                (with-locale (locale *fallback-locale-for-functional-localizations*)
                                  (lookup-resource name :arguments args
                                                   :otherwise [error "Functional localization ~S is missing even for the fallback locale ~A" name (current-locale)])))))
