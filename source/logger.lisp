;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def logger log ())

(def logger rerl (log))
(def logger threads (log))
(def logger timer (log))

(def logger http (rerl))
(def logger app (rerl))
(def logger server (rerl))

(def logger cgi (server))
(def logger files (server))

(def logger l10n (app))
