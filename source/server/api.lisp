;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (generic e) startup-server (server &key &allow-other-keys))
(def (generic e) startup-server/with-lock-held (server &key &allow-other-keys))

(def (generic e) shutdown-server (server &key &allow-other-keys))

(def (generic e) handle-request (thing request)
  (:documentation "The toplevel protocol from which HTTP request handling starts out. Certain parts of the framework start out new protocol branches from this one (e.g. CALL-IF-MATCHES-REQUEST and PRODUCE-RESPONSE for brokers)."))

(def (generic e) startup-broker (broker)
  (:method (broker)
    ))

(def (generic e) shutdown-broker (broker)
  (:method (broker)
    ))

(def generic call-if-matches-request (broker request thunk)
  (:documentation "Should (funcall thunk) if BROKER can handle this REQUEST, and return #f otherwise. The protocol is constructed like this, because it makes it possible to wrap the dynamic extent of the call to PRODUCE-RESPONSE from customized CALL-IF-MATCHES-REQUEST methods.")
  (:method (broker request thunk)
    (error "Default CALL-IF-MATCHES-REQUEST was reached on broker ~S." broker)))

(def (generic e) produce-response (thing request)
  (:documentation "Called from CALL-IF-MATCHES-REQUEST to produce a response object. Besides response objects, it's legal to return from here with both NIL or a list of other brokers to query for response."))
