;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

;; http://www.ietf.org/rfc/rfc3875

(in-package :hu.dwim.web-server)

;; TODO add a timeout for the spawned processes and kill/abandon them if they don't finish in time

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
   (redirect-for-trailing-slash
    #f
    :type boolean
    :accessor redirect-for-trailing-slash?
    :documentation "Some CGI scripts (e.g. mailman) use relative links, so executing them from example.com/mailman/admin will render a ../create link, which will be broken without a trailing slash for the admin url. Enabling this option will redirect to an url with a trailing slash if it's not there already.")
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
  (bind ((script-path (join-strings (list* "/" *matched-query-path-elements*) #\/))
         (extra-path (subseq script-path (length script-path))))
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

(def (function e) make-cgi-directory-broker (path root-directory &key priority (environment '()))
  (make-instance 'cgi-file-broker
                 :path path
                 :root-directory root-directory
                 :priority priority
                 :environment environment))

(def function build-cgi-command-line (broker cgi-file)
  (aif (command-line-transformer-of broker)
       (funcall it cgi-file)
       (list (iolib.pathnames:file-path-namestring cgi-file))))

;; TODO maybe customize the lower layers of file serving to enable some form of caching?
(def method produce-response/directory-serving ((broker cgi-directory-broker) (path list) (relative-path list)
                                                (root-directory iolib.pathnames:file-path))
  (when relative-path
    (bind ((cgi-file-name (first relative-path))
           (extra-path (rest relative-path))
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
              (bind ((script-path (join-strings (append (list* "/" *matched-query-path-elements*) cgi-file-name) #\/))
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
      (set "REQUEST_URI"     (raw-uri-of *request*))
      (set "REQUEST_METHOD"  (http-method-of *request*))
      (set "SCRIPT_NAME"     script-path)
      (set "SERVER_NAME"     (host-of request-uri))
      (set "SERVER_PORT"     (aif (port-of request-uri)
                                  (integer-to-string it)
                                  "80"))
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

(def (function ed) handle-cgi-request (cgi-command-line script-path &key extra-path www-root environment (timeout 30))
  (flet ((delete-file-if-exists (pathspec)
           (handler-case
               (iolib.os:delete-files pathspec)
             (iolib.syscalls:enoent ()
               (values))))
         (file-length* (pathspec)
           (isys:stat-size (isys:stat (iolib.pathnames:file-path-namestring pathspec))))
         (print-environment-to-string (env)
           (with-output-to-string (*standard-output*)
             (maphash (lambda (name value)
                        (princ name)
                        (write-string "=")
                        (princ value)
                        (terpri))
                      (iolib.os::environment-variables env)))))
    (declare (ignorable #'print-environment-to-string))
    (bind ((stdout/file (temporary-file-path "hdws-cgi-stdout"))
           (stderr/file (temporary-file-path "hdws-cgi-stderr")))
      (cgi.dribble "Executing CGI file ~S, matched on script-path ~S, with timeout ~S" cgi-command-line script-path timeout)
      (bind ((final-environment (compute-cgi-environment environment script-path :extra-path extra-path :www-root www-root)))
        (cgi.debug "Executing CGI file matched on script-path ~S, temporary file will be ~S, with timeout ~S.~% * Command line: ~S~% * Input environment:~%     ~S~% * Final environment:~%     ~S. " script-path stdout/file timeout cgi-command-line environment (print-environment-to-string final-environment))
        ;; TODO should be much simpler relying on iolib features, especially the timeout part...
        (bind ((process (iolib.os:create-process cgi-command-line
                                                 :stdin (if (https-request?)
                                                            nil ;; TODO cgi over https needs iolib support, or a pipe. (client-stream/ssl-of *request*)
                                                            (iolib.streams:fd-of (client-stream/iolib-of *request*))) ; pass down a non-blocking fd (can't find anything about blockingness in the standard though)
                                                 :stdout stdout/file
                                                 :stderr stderr/file
                                                 :environment final-environment
                                                 :external-format :utf-8))
               (cleanup-thunk (lambda ()
                                (delete-file-if-exists stdout/file)
                                (delete-file-if-exists stderr/file))))
          (unwind-protect-case ()
              (bind ((exit-code nil)
                     (stderr/length (file-length* stderr/file)))
                (if timeout
                    (iter
                      (with start-time = (get-monotonic-time))
                      (with deadline = (+ start-time timeout))
                      (setf exit-code (iolib.os:process-status process :wait #f))
                      (until (numberp exit-code))
                      (when (> (get-monotonic-time) deadline)
                        (cgi.error "Timeout on CGI file ~S after ~S sec, killing the child process." script-path timeout)
                        (iolib.os:process-kill process iolib.syscalls:sigkill)
                        (return))
                      ;; KLUDGE should not busy wait... but it needs iolib feature.
                      (sleep 0.1))
                    (setf exit-code (iolib.os:process-status process :wait #t)))
                (cgi.debug "Standard output is ~S long, stderr is ~S long" (file-length* stdout/file) stderr/length)
                (if (eql exit-code 0)
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
                      (setf (cleanup-thunk-of it) cleanup-thunk)
                      (setf cleanup-thunk nil))
                    (progn
                      (when exit-code
                        (cgi.info (build-error-log-message :message (format nil "CGI command ~S returned with exit code ~S.~:[~:;~%Error output:~%~S~]"
                                                                            cgi-command-line exit-code
                                                                            (not (zerop stderr/length))
                                                                            ;; TODO add :start & :end limit to READ-FILE-INTO-STRING
                                                                            (subseq-if-longer 1024 (read-file-into-string
                                                                                                    (iolib.pathnames:file-path-namestring stderr/file))
                                                                                              :postfix "[...]"))
                                                           :include-backtrace #f)))
                      (make-raw-functional-response ()
                        (emit-http-response/internal-server-error)))))
            (:always
             (close process))
            (:abort
             (awhen cleanup-thunk
               (funcall it)))))))))
