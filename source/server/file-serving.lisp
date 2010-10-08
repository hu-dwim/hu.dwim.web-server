;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; TODO use iolib for file operations

;;;;;;
;;; Directory serving broker cache entry

(def class* directory-serving-broker/cache-entry ()
  ((file-path)
   (file-write-date)
   (bytes-to-respond)
   (last-updated-at (get-monotonic-time))
   (last-used-at (get-monotonic-time))))

(def function make-directory-serving-broker/cache-entry (file-path &optional file-write-date bytes-to-respond)
  (make-instance 'directory-serving-broker/cache-entry
                 :file-path file-path
                 :file-write-date file-write-date
                 :bytes-to-respond bytes-to-respond))

;;;;;;
;;; Directory serving broker

;; TODO managed in-memory caching of files inside a directory-serving-broker

(def class* directory-serving-broker (broker-at-path-prefix)
  ((root-directory)
   ;; TODO (files-only #f)
   ;; FIXME: there's a threading issue with file-path->cache-entry. access to the broker state is not serialized! fix and audit all subclasses, too...
   (file-path->cache-entry (make-hash-table :test 'equal))
   (path-does-not-exists-response-factory (lambda (&key &allow-other-keys)
                                            (make-not-found-response)))))

(def (function e) make-directory-serving-broker (path-prefix root-directory &key priority)
  (make-instance 'directory-serving-broker :path-prefix path-prefix :root-directory root-directory :priority priority))

(def method produce-response ((broker directory-serving-broker) (request request))
  (bind ((root-directory (root-directory-of broker))
         (path-prefix (path-prefix-of broker))
         (relative-path (remaining-path-of-request-uri request)))
    (server.debug "PRODUCE-RESPONSE for path-prefix ~S, relative-path ~S, root-directory ~A" path-prefix relative-path root-directory)
    (or (make-file-serving-response-for-query-path broker path-prefix relative-path root-directory)
        (funcall (path-does-not-exists-response-factory-of broker)
                 :broker broker
                 :path-prefix path-prefix
                 :relative-path relative-path
                 :root-directory root-directory))))

(def generic make-file-serving-response-for-query-path (broker path-prefix relative-path root-directory)
  (:method ((broker directory-serving-broker) path-prefix relative-path root-directory)
    (bind ((pathname (merge-pathnames relative-path root-directory))
           (truename (ignore-errors
                       (probe-file pathname))))
      (when truename
        (make-file-serving-response-for-directory-entry broker truename path-prefix relative-path root-directory)))))

(def generic make-file-serving-response-for-directory-entry (broker truename path-prefix relative-path root-directory)
  (:method ((broker directory-serving-broker) truename path-prefix relative-path root-directory)
    (files.debug "Making file serving response for ~A, path-prefix ~S, relative-path ~S, root-directory ~S" truename path-prefix relative-path root-directory)
    (cond
      ((cl-fad:directory-pathname-p truename)
       (if (or (ends-with #\/ relative-path)
               (zerop (length relative-path)))
           (make-directory-index-response path-prefix relative-path root-directory truename)
           (make-redirect-response (append-path-to-uri (clone-request-uri) "/"))))
      ((and (default-response-compression)
            (compress-file-before-serving? truename))
       (bind ((compressed-file (shadow-temporary-filename root-directory relative-path "wui-compressed-static-file-cache")))
         (ensure-directories-exist compressed-file)
         (if (and (cl-fad:file-exists-p compressed-file)
                  ;; TODO use iolib for file-write-date?
                  (<= (file-write-date truename)
                      (file-write-date compressed-file)))
             (files.debug "Serving file from the compressed file cache: ~A" compressed-file)
             (progn
               (files.dribble "Updating compressed file cache for ~S, into compressed file ~S" truename compressed-file)
               (with-open-file (input truename :direction :input :element-type '(unsigned-byte 8))
                 (with-open-file (output compressed-file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
                   (hu.dwim.util:deflate
                       (lambda (buffer start size)
                         (read-sequence buffer input :start start :end size))
                       (lambda (buffer start size)
                         (write-sequence buffer output :start start :end size)))
                   (finish-output output)
                   (bind ((input-length (file-length input))
                          (output-length (file-length output)))
                     (assert output-length)
                     (files.debug "Updated compressed file cache for ~S, the compressed file is ~S. Old size ~A, new size ~A, ratio ~,3F" truename compressed-file input-length output-length (/ output-length input-length)))))))
         (aprog1
             (make-file-serving-response compressed-file)
           (setf (header-value it +header/content-encoding+) +content-encoding/deflate+))))
      (t
       (make-file-serving-response truename)))))

(def special-variable *file-compression-extension-blacklist*
    (aprog1
        (make-hash-table :test #'equal)
      (dolist (el '("png" "jpg" "jpeg" "gif"))
        (setf (gethash el it) t))))

(def function compress-file-before-serving? (file)
  (check-type file pathname)
  (not (gethash (pathname-type file) *file-compression-extension-blacklist*)))

;;;;;;
;;; File serving response

(def class* file-serving-response (response)
  ((file-name)
   (delete-file-when-finished #f :type boolean :accessor delete-file-when-finished?)))

(def (function e) make-file-serving-response (file-name &key (delete-when-finished #f))
  (make-instance 'file-serving-response :file-name file-name :delete-file-when-finished delete-when-finished))

(def method send-response ((self file-serving-response))
  (server.info "Sending file serving response from ~S" (file-name-of self))
  (serve-file (file-name-of self)
              :headers (headers-of self)
              :cookies (cookies-of self)))

(def method close-response :after ((self file-serving-response))
  (when (delete-file-when-finished? self)
    (delete-file (file-name-of self))))

(def method convert-to-primitive-response ((response file-serving-response))
  response)

;;;;;;
;;; Directory index

(def class* directory-index-response (response)
  ((path-prefix)
   (root-directory)
   (relative-path)
   (directory)))

(def (function e) make-directory-index-response (path-prefix relative-path root-directory
                                                 &optional (directory (merge-pathnames relative-path root-directory)))
  (aprog1
      (make-instance 'directory-index-response
                     :path-prefix path-prefix :root-directory root-directory
                     :relative-path relative-path :directory directory)
    (setf (header-value it +header/content-type+) (content-type-for +html-mime-type+ (encoding-name-of it)))))

(def method convert-to-primitive-response ((self directory-index-response))
  (bind ((title (string+ "Directory index of \"" (relative-path-of self) "\" under \"" (path-prefix-of self) "\""))
         (body (with-output-to-sequence (*xml-stream* :external-format (external-format-of self)
                                                      :initial-buffer-size 256)
                 (emit-html-document (:content-type +html-content-type+ :title title)
                   (render-directory-as-html (directory-of self) (path-prefix-of self) (relative-path-of self))))))
    (make-byte-vector-response* body
                                :headers (headers-of self)
                                :cookies (cookies-of self))))

(def function render-directory-as-html/default-filter (&key pathname &allow-other-keys)
  ;; by default exclude .foo, both files and dirs
  (or (not (null (pathname-name pathname)))
      (not (starts-with #\. (lastcar (pathname-directory pathname))))))

(def function render-directory-as-html (directory path-prefix relative-path &key (filter 'render-directory-as-html/default-filter))
  <table
    ,@(labels ((render-link (path name)
                 <a (:href ,(escape-as-uri path) :class ,path)
                    ,name>)
               (render-file (file)
                 (bind ((name (apply 'string+ (pathname-name file)
                                     (awhen (pathname-type file)
                                       (list "." it)))))
                   <tr <td ,(render-link (string+ name) name)>
                       <td ,(integer-to-string (isys:stat-size (isys:lstat (namestring file))))>>))
               (render-directory (directory &key
                                            (path (string+ (lastcar (pathname-directory directory)) "/"))
                                            (name path))
                 <tr <td ,(render-link path name)>
                     <td>>))
        (bind ((elements (cl-fad:list-directory directory))
               ((:values directories files)
                (iter (for element :in elements)
                      (when (funcall filter :pathname element)
                        (if (cl-fad:directory-pathname-p element)
                            (collect element :into dirs)
                            (collect element :into files)))
                      (finally (return (values dirs files))))))
          <tr <td ,(render-link (string+ "/" path-prefix relative-path "../") "[parent directory]")>
              <td>>
          (foreach #'render-directory directories)
          (foreach #'render-file files)))>)

;;;;;;
;;; MIME stuff for serving static files

(def special-variable *mime-type->extensions* (make-hash-table :test #'equal))
(def special-variable *extension->mime-types* (make-hash-table :test #'equal))
(def constant +mime-types-file+ #P"/etc/mime.types")

(def function ensure-mime-types-are-read ()
  (when (and (zerop (hash-table-count *mime-type->extensions*))
             (probe-file +mime-types-file+))
    (read-mime-types-file +mime-types-file+)))

(def function parse-mime-types-file (mime-types-file visitor)
  "Parser for /etc/mime.types"
  (iter
    (for line :in-file mime-types-file :using #'read-line)
    (when (or (string= "" line)
              (eq #\# (aref line 0)))
      (next-iteration))
    (for pieces = (remove-if (lambda (piece)
                               (zerop (length piece)))
                             (cl-ppcre:split #.(format nil "( |~A)" #\Tab) line))) ; make that tab char survive tab killing zealots
    (unless (null pieces)
      (funcall visitor pieces))))

(def function read-mime-types-file (mime-types-file)
  "Read in /etc/mime.types file."
  (clrhash *mime-type->extensions*)
  (clrhash *extension->mime-types*)
  (parse-mime-types-file
   mime-types-file
   (lambda (pieces)
     (bind (((type &rest extensions) pieces))
       (setf (gethash type *mime-type->extensions*)
             (append extensions
                     (gethash type *mime-type->extensions*)))
       (dolist (extension extensions)
         (setf (gethash extension *extension->mime-types*)
               (list* type (gethash extension *extension->mime-types*))))))))

(def (function e) extensions-for-mime-type (mime-type)
  "Extensions that can be given to file of given MIME type."
  (check-type mime-type string)
  (gethash mime-type *mime-type->extensions*))

(def (function e) mime-types-for-extension (extension)
  "MIME types associated with the given file extension."
  (check-type extension string)
  (gethash extension *extension->mime-types*))

(with-simple-restart (continue "Ignore the error and continue without reading ~A" +mime-types-file+)
  (ensure-mime-types-are-read))
