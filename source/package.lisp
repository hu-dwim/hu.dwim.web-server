;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.web-server
  (:use :babel
        :babel-streams
        :bordeaux-threads
        :cl-l10n
        :cl-l10n.lang
        :contextl
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.computed-class
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.delico
        :hu.dwim.logger
        :hu.dwim.quasi-quote
        :hu.dwim.quasi-quote.js
        :hu.dwim.quasi-quote.xml
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :trivial-garbage)

  (:shadow #:|defun|
           #:build-error-log-message
           #:log)

  (:export #:$)

  (:shadowing-import-from :trivial-garbage
                          #:make-hash-table)

  (:shadowing-import-from :hu.dwim.syntax-sugar
                          #:define-syntax))

(def package :hu.dwim.web-server.semi-public)

(in-package :hu.dwim.web-server)

;; TODO the interdependency and therefore this list should be much shorter
(def special-variable *semi-public-symbols*
  '(+action-id-parameter-name+
    +frame-id-parameter-name+
    +frame-index-parameter-name+
    +ajax-aware-parameter-name+
    +delayed-content-parameter-name+
    +modifier-keys-parameter-name+
    +scroll-x-parameter-name+
    +scroll-y-parameter-name+

    +action-id-length+
    +frame-id-length+
    +frame-index-length+
    +session-id-length+

    *inside-user-code*
    *rendering-phase-reached*
    *debug-client-side*
    debug-client-side
    debug-on-error
    *profile-request-processing*

    headers-of
    close-response
    send-response
    delayed-content-request?
    ajax-aware-request?

    broker
    broker-based-server
    broker-at-path
    broker-at-path-prefix
    path-of
    path-prefix-of

    session
    frame
    frame-id->frame-of
    frame-index-of
    next-frame-index-of
    mark-frame-invalid
    make-uri-for-new-frame
    session-of
    application-of
    entry-points-of
    id-of

    unique-counter
    unique-counter-of

    default-locale-of
    decorate-session-cookie
    computed-universe-of
    debug-component-hierarchy? ;; TODO it should be in hu.dwim.presentation
    debug-client-side?
    profile-request-processing?
    +default-encoding+
    action-id->action-of
    client-state-sink-id->client-state-sink-of
    cookies-of
    emit-xhtml-prologue
    encoding-name-of
    external-format-of
    guess-encoding-for-http-response
    render-dojo-widget
    identify-http-user-agent
    insert-with-new-random-hash-table-key
    make-functional-response/ajax-aware-client
    make-http-user-agent-breakdown
    with-xhtml-body-environment
    render-action-js-event-handler

    app.dribble
    app.debug
    app.info

    ;; TODO most of these should be public, and probably moved to cl-l10n
    apply-localization-function
    localized-class-name
    localized-enumeration-member
    member-value-name/for-localization-entry
    supported?
    ))

(bind ((semi-public-package (find-package :hu.dwim.web-server.semi-public)))
  (dolist (symbol *semi-public-symbols*)
    (import symbol semi-public-package)
    (export symbol semi-public-package)))
