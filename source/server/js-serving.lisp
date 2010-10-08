;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; js-directory-serving-broker

(def (class* e) js-directory-serving-broker (directory-serving-broker)
  ()
  (:default-initargs
   :render-directory-index #f))

(def (function e) make-js-directory-serving-broker (path-prefix root-directory &key priority)
  (make-instance 'js-directory-serving-broker
                 :path-prefix path-prefix
                 :root-directory root-directory
                 :priority priority))

(def method make-file-serving-response-for-query-path ((broker js-directory-serving-broker) (path-prefix string) (relative-path string)
                                                       (root-directory iolib.pathnames:file-path))
  (assert (not (starts-with #\/ relative-path)))
  (when (ends-with-subseq ".js" relative-path)
    (bind ((relative-path/lisp (string+ (subseq relative-path 0 (- (length relative-path) 2))
                                        "lisp"))
           (absolute-file-path (ignore-errors
                                 (iolib.os:resolve-file-path relative-path/lisp :defaults root-directory)))
           ((:values exists? kind) (when absolute-file-path
                                     (iolib.os:file-exists-p absolute-file-path))))
      (files.dribble "Looking for file ~A, absolute-file-path ~A, exists? ~S, kind ~S, in ~A" relative-path absolute-file-path exists? kind broker)
      (when (and exists?
                 (not (eq kind :directory)))
        (make-file-serving-response-for-directory-entry broker absolute-file-path path-prefix relative-path root-directory)))))

(def method make-directory-serving-broker/cache-key ((broker js-directory-serving-broker) (absolute-file-path iolib.pathnames:file-path) content-encoding)
  (list (iolib.pathnames:file-path-namestring absolute-file-path) content-encoding *debug-client-side*))

(def method update-directory-serving-broker/cache-entry ((broker js-directory-serving-broker) (cache-entry directory-serving-broker/cache-entry)
                                                         (absolute-file-path iolib.pathnames:file-path) (relative-path string) (root-directory iolib.pathnames:file-path)
                                                         response-compression)
  (bind (((:slots bytes-to-respond last-used-at content-encoding file-write-date) cache-entry))
    (files.debug "Compiling js file for the cache entry of ~A, in ~A" absolute-file-path broker)
    (setf bytes-to-respond (compile-js-file-to-byte-vector broker absolute-file-path :encoding :utf-8))
    (setf (values bytes-to-respond content-encoding)
          (compress-response/sequence bytes-to-respond :compression response-compression))
    (files.debug "Compiled and compressed js file for the cache entry of ~A, in ~A. Content encoding is ~S." absolute-file-path broker content-encoding)))

(def generic compile-js-file-to-byte-vector (broker filename &key encoding)
  (:method ((broker js-directory-serving-broker) (filename iolib.pathnames:file-path) &key (encoding +default-encoding+))
    (bind ((body-as-string (read-file-into-string (iolib.pathnames:file-path-namestring filename) :external-format encoding)))
      (setf body-as-string (string+ "`js(progn "
                                               body-as-string
                                               ")"))
      (bind ((*package* (find-package :hu.dwim.wui)))
        (with-local-readtable
          (setup-readtable)
          (enable-js-sharpquote-syntax)
          (coerce-to-simple-ub8-vector (with-output-to-sequence (*js-stream*)
                                         (bind ((forms (read-from-string body-as-string)))
                                           (eval forms)))))))))

;;;;;;
;;; js-component-hierarchy-broker

(def constant +js-component-hierarchy-serving-broker/default-path+ "/wui/js/component-hierarchy.js")

(def special-variable *js-component-hierarchy-cache* nil)

(def special-variable *js-component-hierarchy-cache/last-modified-at* (local-time:now))

(def (class* e) js-component-hierarchy-serving-broker (broker-at-path)
  ()
  (:default-initargs :path +js-component-hierarchy-serving-broker/default-path+))

(def function clear-js-component-hierarchy-cache ()
  (setf *js-component-hierarchy-cache* nil)
  (setf *js-component-hierarchy-cache/last-modified-at* (local-time:now)))

(def method produce-response ((self js-component-hierarchy-serving-broker) request)
  (make-byte-vector-response* (or *js-component-hierarchy-cache*
                                  (setf *js-component-hierarchy-cache*
                                        (emit-into-js-stream-buffer (:external-format :utf-8)
                                          (serve-js-component-hierarchy))))
                              :last-modified-at *js-component-hierarchy-cache/last-modified-at*
                              :seconds-until-expires (* 60 60)
                              :content-type (content-type-for +javascript-mime-type+ :utf-8)))

;; FIXME: this generates a quite big list which is redundant like hell
;;        but it is cached and compressed, so we don't care about it right now
(def (function e) serve-js-component-hierarchy ()
  (bind ((component-class (find-class 'component)))
    `js(setf wui.component-class-precedence-lists (create))
    (dolist (subclass (subclasses component-class))
      (bind ((subclass-name (class-name-as-string subclass))
             (class-precedence-list (class-precedence-list subclass))
             (index (position component-class class-precedence-list))
             (class-name-precedence-list (mapcar #'class-name-as-string (subseq class-precedence-list 0 (1+ index)))))
        `js(setf (slot-value wui.component-class-precedence-lists ,subclass-name) (array ,@class-name-precedence-list))))
    +void+))
