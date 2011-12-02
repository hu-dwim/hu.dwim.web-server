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

(def (special-variable :documentation "While walking down the broker tree for someone to handle this request, this variable is always rebound to the currently remaining path elements of the request uri.")
  *remaining-query-path-elements*)

(def (special-variable :documentation "Like *REMAINING-QUERY-PATH-ELEMENTS*, but contains the already matched elements.")
  *matched-query-path-elements*)
