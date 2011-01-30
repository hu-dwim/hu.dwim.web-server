;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

;; http://www.ietf.org/rfc/rfc3875

(in-package :hu.dwim.web-server)

(def (class* e) cgi-broker ()
  ((environment
    '()
    :documentation "An alist of (name . value) pairs specifying environment variables for the CGI file.")
   (effective-user-id
    nil
    :type (or null integer string)
    :documentation "Posix user to run the CGI file with.")
   (effective-group-id
    nil
    :type (or null integer string)
    :documentation "Posix group to run the CGI file with.")
   (command-line-transformer
    nil
    :type (or null function-designator)
    :documentation "Will be invoked with the IOLIB.PATHNAMES:FILE-PATH of the CGI executable, and must produce a list of strings passed on to IOLIB.OS:RUN-PROGRAM.")
   (www-root
    nil
    :type (or null iolib.pathnames:file-path-designator)
    :documentation "The basis of the virtual-to-physical translation when calculating the PATH_TRANSLATED CGI environment variable."))
  (:documentation "The base class for CGI serving."))

;;;;;;
;;; broker for a single CGI file

(def (class* e) cgi-file-broker (cgi-broker broker-at-path)
  ((cgi-file :type iolib.pathnames:file-path-designator))
  (:documentation "A broker with a specific path for a single CGI file."))

(def (function e) make-cgi-file-broker (path cgi-file &key priority environment)
  (make-instance 'cgi-file-broker
                 :path path
                 :cgi-file cgi-file
                 :priority priority
                 :environment environment))

(def method produce-response ((broker cgi-file-broker) request)
  (bind ((script-path (apply #'string+ (reverse *matching-uri-path-element-stack*)))
         (extra-path (subseq (path-of (uri-of *request*)) (length script-path))))
    (handle-cgi-request (build-cgi-command-line broker (cgi-file-of broker)) script-path
                        :extra-path extra-path
                        :www-root (www-root-of broker)
                        :environment (environment-of broker))))

;;;;;;
;;; broker for a CGI directory prefix

(def (class* e) cgi-directory-broker (cgi-broker directory-serving-broker)
  ((root-directory :type iolib.pathnames:file-path-designator))
  (:default-initargs
   :render-directory-index #f)
  (:documentation "A broker to serve all executable CGI files in a directory."))

(def (function e) make-cgi-directory-broker (path-prefix root-directory &key priority (environment '()))
  (make-instance 'cgi-file-broker
                 :path-prefix path-prefix
                 :root-directory root-directory
                 :priority priority
                 :environment environment))

(def function build-cgi-command-line (broker cgi-file)
  (aif (command-line-transformer-of broker)
       (funcall it cgi-file)
       (list (iolib.pathnames:file-path-namestring cgi-file))))

;; TODO maybe customize the lower layers of file serving to enable some form of caching?
(def method produce-response/directory-serving ((broker cgi-directory-broker) (path-prefix string) (relative-path string)
                                                (root-directory iolib.pathnames:file-path))
  (assert (not (starts-with #\/ relative-path)))
  (unless (string= relative-path "")
    (bind (((:values cgi-file-name extra-path) (aif (position #\/ relative-path)
                                                    (values (subseq relative-path 0 it)
                                                            (subseq relative-path it))
                                                    (values relative-path nil)))
           (cgi-file (iolib.pathnames:merge-file-paths cgi-file-name root-directory))
           (exists? (iolib.os:file-exists-p cgi-file))
           (follow-symlinks? (allow-access-to-external-files? broker))
           (kind (iolib.os:file-kind cgi-file :follow-symlinks follow-symlinks?))
           (effective-user-id (or (effective-user-id-of broker)
                                  (iolib.syscalls:getuid)))
           (effective-group-id (or (effective-group-id-of broker)
                                   (iolib.syscalls:getgid))))
      (cgi.debug "Inspecting file ~A, kind ~S, effective uid ~S, gid ~S" cgi-file kind effective-user-id effective-group-id)
      (if (and exists?
               (eq kind :regular-file))
          (if (is-file-executable? cgi-file :follow-symlinks follow-symlinks? :effective-user-id effective-user-id :effective-group-id effective-group-id)
              (bind ((script-path (apply #'string+ (append (reverse *matching-uri-path-element-stack*) (list cgi-file-name))))
                     (cgi-file (iolib.os:absolute-file-path cgi-file))
                     (cgi-command-line (build-cgi-command-line broker cgi-file)))
                (handle-cgi-request cgi-command-line script-path
                                    :extra-path extra-path
                                    :www-root (www-root-of broker)
                                    :environment (environment-of broker)))
              (cgi.debug "NOT serving ~A as a CGI file because its not executable by the requested effective uid ~S and gid ~S" cgi-file effective-user-id effective-group-id))
          (cgi.debug "NOT serving ~A as a CGI file because its kind is ~S" cgi-file kind)))))

;;;;;;
;;; actual CGI magic

(def constant +static-cgi-environment+ '(("GATEWAY_INTERFACE" . "CGI/1.1")
                                         ("SERVER_SOFTWARE" . "hu.dwim.web-server")))

(def function compute-cgi-environment (start-environment script-path &key extra-path www-root)
  (bind ((request-uri (uri-of *request*))
         ;;(request-uri-path (path-of request-uri))
         (environment (make-instance 'iolib.os:environment)))
    (labels ((set (name value)
               (setf (iolib.os:environment-variable name environment) value))
             (slurp (alist)
               (loop
                 :for (name . value) :in alist
                 :collect (set name value))))
      (slurp start-environment)
      (slurp +static-cgi-environment+)
      (when (and www-root
                 extra-path)
        (set "PATH_TRANSLATED" (string+ (iolib.pathnames:file-path-namestring www-root) extra-path)))
      (awhen (nth-value 2 (handler-case
                              (iolib.sockets:lookup-hostname *request-remote-address*)
                            (serious-condition ()
                              ;; lookup-hostname signals when something is not found
                              (values))))
        (set "REMOTE_HOST" it))
      (set "PATH_INFO"       extra-path)
      (set "QUERY_STRING"    (query-of request-uri))
      (set "REMOTE_ADDR"     *request-remote-address/string*)
      (set "REQUEST_METHOD"  (http-method-of *request*))
      (set "SCRIPT_NAME"     script-path)
      (set "SERVER_NAME"     (host-of request-uri))
      (set "SERVER_PORT"     (or (port-of request-uri) "80"))
      (set "SERVER_PROTOCOL" (http-version-string-of *request*))

      ;; (set "AUTH_TYPE")
      ;; (set "REMOTE_IDENT") optional
      ;; (set "REMOTE_USER") TODO

      ;; TODO hrm, currently we parse the entire request unconditionally... what about the next two?
      ;; (set "CONTENT_LENGTH") TODO
      ;; (set "CONTENT_TYPE") TODO

      ;; TODO should set HTTP_ variables for the request headers modulo well-known ones already processed by the http server
      )
    environment))

(def (function ed) handle-cgi-request (cgi-command-line script-path &key extra-path www-root environment)
  (flet ((delete-file-if-exists (pathspec)
           (when (iolib.os:file-exists-p pathspec)
             (iolib.os:delete-files pathspec)))
         (file-length* (pathspec)
           (isys:stat-size (isys:stat (iolib.pathnames:file-path-namestring pathspec)))))
    (bind ((stdout/file (iolib.pathnames:file-path (filename-for-temporary-file "hdws-cgi-stdout")))
           (stderr/file (iolib.pathnames:file-path (filename-for-temporary-file "hdws-cgi-stderr"))))
      (cgi.debug "Executing CGI file ~S, matched on script-path ~S" cgi-command-line script-path)
      (bind ((final-environment (compute-cgi-environment environment script-path :extra-path extra-path :www-root www-root)))
        (cgi.dribble "Executing CGI file matched on script-path ~S, temporary file will be ~S.~% * Command line: ~S~% * Input environment:~%     ~S~% * Final environment:~%     ~S. " script-path stdout/file cgi-command-line environment final-environment)
        (bind ((process (iolib.os:create-process cgi-command-line
                                                 :stdout stdout/file
                                                 :stderr stderr/file
                                                 :environment final-environment
                                                 :external-format :utf-8)))
          (unwind-protect-case ()
              (bind ((exit-code (iolib.os:process-status process :wait #t))
                     #+nil
                     (stderr (with-output-to-string (str)
                               (copy-stream (iolib.os:process-stderr process) str))))
                (cgi.dribble "Standard output is ~S long, stderr is ~S long" (file-length* stdout/file) (file-length* stderr/file))
                (unless (zerop exit-code)
                  (cgi.error (build-error-log-message :message (format nil "CGI command ~S returned with exit code ~S" cgi-command-line exit-code)
                                                      :include-backtrace #f)))
                (aprog1
                    (make-raw-functional-response ()
                      (bind ((stream (client-stream-of *request*)))
                        (write-sequence #.(string-to-us-ascii-octets "HTTP/1.1 ") stream)
                        (write-sequence (string-to-us-ascii-octets "200 OK") stream)
                        (write-byte +space+ stream)
                        ;; TODO do it smarter than this... copystream it or sendfile it.
                        (bind ((contents (read-file-into-byte-vector (iolib.pathnames:file-path-namestring stdout/file))))
                          (cgi.debug "Emitting ~S bytes of response generated by CGI command ~S into temp file ~S" (length contents) cgi-command-line stdout/file)
                          (write-sequence contents stream))))
                  (setf (cleanup-thunk-of it) (lambda ()
                                                (delete-file-if-exists stdout/file)))))
            (:abort
             (delete-file-if-exists stdout/file)
             (delete-file-if-exists stderr/file))))))))
