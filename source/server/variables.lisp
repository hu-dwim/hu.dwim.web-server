;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def special-variable *profile-request-processing* #f
  "Should the the statistical profiler be enabled while inside the request processing body?")

(def generic profile-request-processing? (context)
  (:method ((context t))
    *profile-request-processing*))

(def special-variable *debug-client-side* (not *load-as-production?*)
  "Should the client side run in debug mode?")

(def (generic e) debug-client-side? (context)
  (:method ((context t))
    *debug-client-side*))

(def (special-variable e :documentation "The SERVER associated with the currently processed HTTP REQUEST.")
  *server*)

(def (special-variable :documentation "Holds the broker path while processing the rules. Whenever a broker provides a new set of rules to dispatch on, it is pushed at the head of the *BROKER-STACK* list.")
  *broker-stack*)

(def (special-variable :documentation "If HANDLE-TOPLEVEL-ERROR gets called then this will be its context argument.")
  *context-of-error* nil)

(def (special-variable :documentation "A stack of currently matching path elements while request handling is going deeper and deeper in the broker tree.")
  *matching-uri-path-element-stack*)

(def (special-variable :documentation "Optimization; keeps the current value of (reduce #'+ *matching-uri-path-element-stack* :key #'length).")
  *matching-uri-path-element-stack/total-length*)

(def (special-variable :documentation "Optimization; it's either NIL or the current value of (subseq (path-of (uri-of request)) *matching-uri-path-element-stack/total-length*).")
  *matching-uri-path-element-stack/remaining-path*)

(def function remaining-path-of-request-uri (&optional (request *request*))
  "While matching the request uri, returns the remaining part of the path of the uri that have not yet been matched."
  (or *matching-uri-path-element-stack/remaining-path*
      ;; TODO this is slow and WTF... need to find out how it should work.
      (bind ((uri-path (with-output-to-string (str)
                         (iter (for el :in (path-of (uri-of request)))
                               (write-char #\/ str)
                               (write-string el str)))))
        (setf *matching-uri-path-element-stack/remaining-path*
              (subseq uri-path *matching-uri-path-element-stack/total-length*)))))

(def with-macro with-new-matching-uri-path-element (path)
  (check-type path string)
  (bind ((*matching-uri-path-element-stack* (cons path *matching-uri-path-element-stack*))
         (*matching-uri-path-element-stack/total-length* (+ (length path) *matching-uri-path-element-stack/total-length*))
         (*matching-uri-path-element-stack/remaining-path* nil))
    (-body-)))
