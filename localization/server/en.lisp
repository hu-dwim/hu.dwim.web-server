;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def localization en
  (class-name.request "request")
  (class-name.response "response")

  (slot-name.http-header "http header")
  (slot-name.kind "kind")
  (slot-name.version "version")
  (slot-name.supported "supported")
  (slot-name.count "count"))
