;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def function decorate-session-cookie (application response)
  ;; this function is only called when we are sending back a response after creating a session
  (bind ((hostname (host-of (uri-of *request*))))
    ;; TODO assert against double additions?
    (app.debug "Decorating response ~A with the session cookie for session ~S" response *session*)
    (add-cookie (make-cookie
                 +session-cookie-name+
                 (aif *session*
                      (id-of it)
                      "")
                 :max-age (unless *session*
                            0)
                 :comment "hu.dwim.web-server session id"
                 :domain (if (find #\. hostname)
                             (string+ "." hostname)
                             (progn
                               (app.warn "Domain used to reach the server is illegal (no dot character in ~S), not providing a domain argument for the session cookie" hostname)
                               nil))
                 :path (string+ "/" (join-strings (path-of application) #\/)))
                response))
  response)

(def function ajax-aware-request? (&optional (request *request*))
  "Did the client js side code notify us that it's ready to receive ajax answers?"
  (bind ((value (request-parameter-value request +ajax-aware-parameter-name+)))
    (and value
         (etypecase value
           (cons (some [not (string= !1 "")] value))
           (string (not (string= value "")))))))

(def function delayed-content-request? (&optional (request *request*))
  "A delayed content request is supposed to render stuff to the same frame that was delayed at the main request (i.e. tooltips)."
  (bind ((value (request-parameter-value request +delayed-content-parameter-name+)))
    (and value
         (etypecase value
           (cons (some [not (string= !1 "")] value))
           (string (not (string= value "")))))))

(def method produce-response ((application application) request)
  (assert (not (boundp '*rendering-phase-reached*)))
  (assert (not (boundp '*inside-user-code*)))
  (assert (eq (first *broker-stack*) application))
  (bind ((*application* application))
    (when (> (- (get-monotonic-time)
                (sessions-last-purged-at-of application))
             +session-purge/time-interval+)
      (purge-sessions application))
    (with-locale (default-locale-of application)
      (bind ((local-time:*default-timezone* (default-timezone-of application))
             ;; bind *session* and *frame* here, so that WITH-SESSION/FRAME/ACTION-LOGIC and entry-points can freely setf it
             (*session* nil)
             (*frame* nil)
             (*application-relative-path* *remaining-query-path-elements*)
             (*fallback-locale-for-functional-localizations* (default-locale-of application))
             (*rendering-phase-reached* #f)
             (*inside-user-code* #f)
             (*debug-client-side* (debug-client-side? application))
             (*ajax-aware-request* (ajax-aware-request?))
             (*delayed-content-request* (or *ajax-aware-request*
                                            (delayed-content-request?))))
        (app.debug "~A matched with *application-relative-path* ~S, querying entry-points for response" application *application-relative-path*)
        (assert (or (not *ajax-aware-request*)
                    *delayed-content-request*))
        (produce-response/application application request)))))

(def method produce-response/application ((application application) request)
  (bind ((response (query-brokers-for-response request (entry-points-of application) :otherwise nil)))
    (when response
      (unwind-protect
           (progn
             (app.debug "Calling SEND-RESPONSE for ~A while still inside the dynamic extent of the PRODUCE-RESPONSE method of application" response)
             (send-response response))
        (close-response response))
      ;; TODO why not unwinding from here instead of make-do-nothing-response?
      (make-do-nothing-response))))

;;;;;;
;;; Error handling

(def method handle-toplevel-error/emit-response ((application application) (error serious-condition))
  (handle-toplevel-error/application/emit-response application error *ajax-aware-request*))

(def method handle-toplevel-error/application/emit-response ((application application) (error serious-condition) (ajax-aware? (eql #t)))
  (app.debug "HANDLE-TOPLEVEL-ERROR/APPLICATION/EMIT-RESPONSE is sending an internal error response for the ajax aware request")
  (emit-response-for-ajax-aware-client ()
    <script `js-inline(hdws.inform-user-about-error "error.internal-server-error"
                                                    :title "error.internal-server-error.title")>))

(def method handle-toplevel-error :before ((application application) (error serious-condition))
  (when (and (not *inside-user-code*)
             *session*)
    ;; oops, looks like this error comes from inside the framework. let's try to invalidate the session...
    (server.warn "Invalidating session ~A because an error came from inside the framework while processing a request coming to this session" *session*)
    (mark-session-invalid *session*)
    (setf *session* nil)
    (setf *frame* nil)))

;;;;;;
;;; Utils

(def (function e) make-redirect-response-with-frame-parameters-decorated (&optional (frame *frame*))
  (bind ((uri (clone-request-uri)))
    (assert (and frame (not (null (id-of frame)))))
    (setf (uri/query-parameter-value uri +frame-id-parameter-name+) (id-of frame))
    (setf (uri/query-parameter-value uri +frame-index-parameter-name+) (frame-index-of frame))
    (make-redirect-response uri)))

(def (function e) make-uri-for-current-application (&optional relative-path)
  (bind ((uri (clone-request-uri)))
    (uri/delete-all-query-parameters uri)
    (decorate-uri-for-current-application uri)
    (when relative-path
      (uri/append-path uri relative-path))
    uri))

(def (function e) decorate-uri-for-current-application (uri)
  (assert *application*)
  (decorate-uri uri *application*)
  (when *session*
    (decorate-uri uri *session*))
  (when *frame*
    (decorate-uri uri *frame*))
  uri)

(def function make-uri-for-current-frame ()
  (bind ((uri (clone-request-uri)))
    (setf (uri/query-parameter-value uri +action-id-parameter-name+) nil)
    (decorate-uri uri *frame*)
    uri))

(def function make-uri-for-new-frame ()
  (bind ((uri (clone-request-uri)))
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (setf (uri/query-parameter-value uri +frame-id-parameter-name+) nil)
    (setf (uri/query-parameter-value uri +frame-index-parameter-name+) nil)
    uri))

(def (function e) make-redirect-response-for-current-application (&optional relative-path)
  (make-redirect-response (make-uri-for-current-application relative-path)))
