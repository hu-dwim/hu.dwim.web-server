;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; specials available while processing a request

(def (special-variable e :documentation "The APPLICATION associated with the currently processed HTTP REQUEST.")
  *application*)

(def (special-variable e :documentation "The SESSION associated with the currently processed HTTP REQUEST.")
  *session*)

(def (special-variable e :documentation "The FRAME associated with the currently processed HTTP REQUEST.")
  *frame*)

(def (special-variable e :documentation "Bound in the dynamic extent of WITH-ACTION-LOGIC. Its value is either NIL or an action if one was successfully associated with the http request.")
  *action*)

(def (special-variable :documentation "It's set to true when the rendering protocol is invoked, and with that it provides information for the error handling code.")
  *rendering-phase-reached*)

(def (special-variable :documentation "It's bound to true while in the dynamic extent of user code (the bodies of entry points, actions and the render methods of components).")
  *inside-user-code*)

(def (special-variable e :documentation "When true, it means that the current request will render a bit more content for the current snapshot (as opposed to do a full redraw). AJAX requests are implicitly delayed content requests.")
  *delayed-content-request*)

(def (special-variable e :documentation "When true, it means that the request was fired by the client side JS stack and expects an XML answer.")
  *ajax-aware-request*)

(def (special-variable e :documentation "Bound inside entry-points, and contains part of the path of the request url that comes after the application's url prefix.")
  *application-relative-path*)

(def (special-variable e :documentation "Bound inside entry-points, and contains part of the path of the request url that comes after the entry-point url prefix (so, it's an empty string for exact path matching entry-point's).")
  *entry-point-relative-path*)

;;;;;;
;;; application slot defaults

;; TODO think through the 'default' prefix. somewhere added, somewhere not... maybe rebind from app context...

(def (special-variable e) *maximum-sessions-per-application-count* most-positive-fixnum
  "The default for the same slot in applications.")

(def (special-variable e) *default-session-timeout* (* 30 60)
  "The default for the same slot in applications.")

(def (special-variable e) *default-frame-timeout* *default-session-timeout*
  "The default for the same slot in applications.")

(def (special-variable e) *default-ajax-enabled* #t
  "The default for the same slot in applications.")

;;;;;;
;;; constants

(def constant +session-purge/time-interval+ 30)

(def constant +action-id-length+   8)
(def constant +frame-id-length+    8)
(def constant +frame-index-length+ 4)
(def constant +session-id-length+  40)

(def constant +action-id-parameter-name+       "_a")
(def constant +frame-id-parameter-name+        "_f")
(def constant +frame-index-parameter-name+     "_x")
(def constant +ajax-aware-parameter-name+      "_j")
(def constant +delayed-content-parameter-name+ "_d")
(def constant +modifier-keys-parameter-name+   "_m")

(pushnew +ajax-aware-parameter-name+ *clone-request-uri/default-strip-query-parameters* :test 'equal)
(pushnew +delayed-content-parameter-name+ *clone-request-uri/default-strip-query-parameters* :test 'equal)

(def constant +frame-query-parameter-names+ (list +frame-id-parameter-name+
                                                  +frame-index-parameter-name+
                                                  +action-id-parameter-name+))

(def constant +session-cookie-name+ "sid")
