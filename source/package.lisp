;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.wui
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

  (:shadow #:class-prototype
           #:class-slots
           #:class-precedence-list
           #:|defun|
           #:build-error-log-message)

  (:shadowing-import-from :trivial-garbage
                          #:make-hash-table)

  (:shadowing-import-from :hu.dwim.syntax-sugar
                          #:define-syntax))
