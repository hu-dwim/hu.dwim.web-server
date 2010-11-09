;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;; we need to do it by hand, because we don't use the :readtable-setup extension of (def package :hu.dwim.web-server ...)
(hu.dwim.def::notify-swank-about-package-readtable :hu.dwim.web-server)
