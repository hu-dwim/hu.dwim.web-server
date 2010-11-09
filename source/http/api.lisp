;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (generic e) handle-toplevel-error (context error)
  (:documentation "Called when a signaled error is about to cross a boundary which it shouldn't.

There's no guarantee when it is called, e.g. maybe *response* has already been constucted and the network stream written to.

CONTEXT is usually (first *brokers*) but can be any contextual information including NIL."))

(def (generic e) handle-toplevel-error/emit-response (context error)
  (:documentation "Called to emit a response in case an error reached toplevel. Called from HANDLE-TOPLEVEL-ERROR."))
