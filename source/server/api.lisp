;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; request

(def (class* eas) request ()
  ())

(def generic close-request (request))


;;;;;;
;;; response

(def (class* eas) response ()
  ())

(def generic send-response (response)
  (:method ((response response))
    (send-response (convert-to-primitive-response response)))
  (:documentation "Emit RESPONSE into the http stream."))

(def generic close-response (response)
  (:documentation "This method is called when RESPONSE is no longer needed."))

(def (class* e) primitive-response (response)
  ()
  (:documentation "Primitive responses are the ones that are ready to be serialized into the network stream and transferred to the client."))

(def (class* e) primitive-http-response (primitive-response http-response)
  ())

(def (generic e) convert-to-primitive-response (response)
  (:documentation "Called on all responses before trying to emit them, and it's supposed to return a PRIMITIVE-RESPONSE.")
  (:method :around (response)
    (bind ((*response* response))
      (call-next-method)))
  (:method ((response primitive-response))
    response)
  (:method ((response null))
    nil))


;;;;;;
;;; HTTP request/response

(def class* http-message ()
  ((headers nil)
   ;; left unbound, which is a marker for the lazy request cookie parser to do the parsing
   (cookies :documentation "An alist cache of the parsed Cookie header value. Its accessor lazily initializes the slot.")))

(def (class* eas) http-request (http-message request)
  ((client-stream :type stream :documentation "A main bivalent output stream that delivers data *to* the http client.")
   (client-stream/iolib :type stream :documentation "The underlying iolib stream.")
   (client-stream/ssl :type stream :documentation "An optional bivalent SSL wrapper stream wrapping CLIENT-STREAM/IOLIB.")
   (keep-alive :initform #t :type boolean :accessor keep-alive?)
   (http-method :type string)
   (http-version-string :type string)
   (http-major-version :type integer)
   (http-minor-version :type integer)
   (raw-uri :type string)
   (uri :type uri)
   (query-parameters :type list :documentation "Holds all the query parameters from the uri and/or the request body")
   (accept-encodings :type list :documentation "An alist cache of the parsed ACCEPT-ENDODINGS header value. Its accessor lazily initializes the slot.")))

(def (function e) https-request? (&optional (request *request*))
  (to-boolean (client-stream/ssl-of request)))

(def (generic e) header-value (http-message header-name)
  (:method ((message http-message) header-name)
    (header-alist-value (headers-of message) header-name)))

(def (generic e) (setf header-value) (value http-message header-name)
  (:method (value (message http-message) header-name)
    (setf (header-alist-value (headers-of message) header-name) value)))

(def generic send-headers (http-response))


;;;;;;
;;; error handling

(def (generic e) handle-toplevel-error (context error)
  (:documentation "Called when a signaled error is about to cross a boundary which it shouldn't.

There's no guarantee when it is called, e.g. maybe *response* has already been constucted and the network stream written to.

CONTEXT is usually (first *brokers*) but can be any contextual information including NIL."))

(def (generic e) handle-toplevel-error/emit-response (context error)
  (:documentation "Called to emit a response in case an error reached toplevel. Called from HANDLE-TOPLEVEL-ERROR."))


;;;;;;
;;; server stuff

(def (generic e) startup-server (server &key &allow-other-keys))
(def (generic e) startup-server/with-lock-held (server &key &allow-other-keys))

(def (generic e) shutdown-server (server &key &allow-other-keys))

(def generic read-request (server stream/iolib stream/ssl)
  (:method :around (server stream/iolib stream/ssl)
    (with-thread-name " / READ-REQUEST"
      (call-next-method))))

(def (generic e) handle-request (thing request)
  (:documentation "The toplevel protocol from which request handling starts out. Certain parts of the framework start out new protocol branches from this one (e.g. CALL-IF-MATCHES-REQUEST and PRODUCE-RESPONSE for brokers)."))

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

(def (generic e) produce-response (broker request)
  (:documentation "Called from CALL-IF-MATCHES-REQUEST to produce a response object. Besides response objects, it's legal to return from here with both NIL or a list of other brokers to query for response."))
