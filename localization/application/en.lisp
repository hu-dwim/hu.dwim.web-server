;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def localization en
  (render-application-internal-error-page (&key administrator-email-address &allow-other-keys)
    <div
      <p "The developers will be notified about this error and they will hopefully fix it soon.">
      ,(when administrator-email-address
         <p "You may contact the administrators at this email address: "
             <a (:href ,(mailto-href administrator-email-address)) ,administrator-email-address>>)>)
  )

(def js-localization en
  (error.ajax.request-to-invalid-session "The connection to the server has timed out. You can reconnect by refreshing the page.")
  (error.ajax.request-to-invalid-frame "TODO error.ajax.request-to-invalid-frame")
  (error.network-error.title "Communication error")
  (error.network-error "There was an error while communicating with the server. This might be due to a transient network error, so please try reloading the page. If the problem persist, then please contact the administrators.")
  (error.generic-javascript-error.title "Unexpected client side error")
  (error.generic-javascript-error "There was an unexpected error on the client side. This might be due to a browser incompatibility, so please make sure your browser is supported and updated. If the problem persist using a supported browser, then please contact the operator.")
  (error.internal-server-error.title "Internal server error")
  (error.internal-server-error "An internal server error has occurred, we are sorry for the inconvenience. Please try again")
  (action.reload-page "Refresh page")
  (action.cancel "Cancel")
  )

(def localization en
  (class-name.application "application")
  (class-name.session "session")
  (class-name.frame "frame")
  (class-name.action "action")
  (class-name.entry-point "entry point")

  (slot-name.debug-on-error "debug on error")
  (slot-name.processed-request-counter "request counter")
  (slot-name.priority "priority")
  (slot-name.requests-to-sessions-count "number of requests to sessions")
  (slot-name.default-uri-scheme "default uri scheme")
  (slot-name.default-locale "default locale")
  (slot-name.supported-locales "supported locales")
  (slot-name.default-timezone "default timezone")
  (slot-name.session-class "session class")
  (slot-name.session-timeout "session timeout")
  (slot-name.frame-timeout "frame timeout")
  (slot-name.sessions-last-purged-at "sessions last purged at")
  (slot-name.maximum-sessions-count "maximum number of sessions")
  (slot-name.session-id->session "session map")
  (slot-name.administrator-email-address "administrator email address")
  (slot-name.running-in-test-mode "running in test mode")
  (slot-name.ajax-enabled "ajax enabled")
  (slot-name.dojo-skin-name "dojo skin name")
  (slot-name.dojo-file-name "dojo file name")
  (slot-name.dojo-directory-name "dojo directory name"))
