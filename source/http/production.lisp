;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; HTTP port

;; KLUDGE move to meta-model or somewhere else because this is not general enough, and can not be project independent: https, or not, starting multiple services on multiple ports, same service listening on multiple ports, etc...
(def (constant e) +default-http-server-port+ 8080)

