;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def special-variable *session-application* (make-instance 'standard-application :path-prefix "/session/"))

(def entry-point (*session-application* :path "")
  (with-entry-point-logic (:requires-valid-session #f :ensure-session #f :requires-valid-frame #f :ensure-frame #t)
    (if *session*
        (progn
          (assert (and (boundp '*frame*)
                       *frame*))
          (make-raw-functional-response ()
            (emit-http-response/simple-html-document ()
              <p "We have a session now... "
                 <span ,(or (root-component-of *frame*)
                            (setf (root-component-of *frame*) "Hello world from a session!"))>
                 <a (:href ,(string+ (path-prefix-of *application*) "delete/"))
                    "delete session">>)))
        (bind ((application *application*)) ; need to capture it in the closure
          (make-raw-functional-response ()
            (emit-http-response/simple-html-document ()
              <p "There's no session... "
                 <a (:href ,(string+ (path-prefix-of application) "new/"))
                    "create new session">>))))))

(def entry-point (*session-application* :path "new/")
  (with-entry-point-logic (:requires-valid-session #f :ensure-session #t :requires-valid-frame #f)
    (make-redirect-response-for-current-application)))

(def entry-point (*session-application* :path "delete/")
  (bind ((old-session nil))
    (with-session-logic ()
      (setf old-session *session*)
      (values))
    (when old-session
      (with-lock-held-on-application (*application*)
        (delete-session *application* old-session)))
    (make-redirect-response (path-prefix-of *application*))))

(def file-serving-entry-point *session-application* "/session/static/" (system-relative-pathname :hu.dwim.web-server.test "www/"))
