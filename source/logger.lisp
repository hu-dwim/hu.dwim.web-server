;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def logger wui ())

(def logger rerl (wui))
(def logger threads (wui))
(def logger timer (wui))
(def logger component (wui))
(def logger incremental (component))

(def logger http (rerl))
(def logger app (rerl))
(def logger server (rerl))

(def logger cgi (server))
(def logger files (server))

(def logger l10n (app))
