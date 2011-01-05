;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

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
  (bind (((:slots bytes-to-respond last-used-at content-encoding content-type file-write-date) cache-entry))
    (files.debug "Compiling js file for the cache entry of ~A, in ~A" absolute-file-path broker)
    (setf bytes-to-respond (compile-js-file-to-byte-vector broker absolute-file-path :encoding :utf-8))
    (setf (values bytes-to-respond content-encoding)
          (compress-response/sequence bytes-to-respond :compression response-compression))
    (setf content-type (content-type-for +javascript-mime-type+))
    (files.debug "Compiled and compressed js file for the cache entry of ~A, in ~A. Content encoding is ~S." absolute-file-path broker content-encoding)))

(def generic compile-js-file-to-byte-vector (broker filename &key encoding)
  (:method ((broker js-directory-serving-broker) (filename iolib.pathnames:file-path) &key (encoding +default-encoding+))
    (bind ((body-as-string (read-file-into-string (iolib.pathnames:file-path-namestring filename) :external-format encoding)))
      (setf body-as-string (string+ "`js(progn "
                                               body-as-string
                                               ")"))
      (bind ((*package* (find-package :hu.dwim.web-server)))
        (with-local-readtable
          (setup-readtable)
          (enable-js-sharpquote-syntax)
          (coerce-to-simple-ub8-vector (with-output-to-sequence (*js-stream*)
                                         (bind ((forms (read-from-string body-as-string)))
                                           (eval forms)))))))))
