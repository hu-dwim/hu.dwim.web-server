;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.web-server.test
  (:use :babel
        :babel-streams
        :cl-l10n
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.logger
        :hu.dwim.quasi-quote
        :hu.dwim.quasi-quote.js
        :hu.dwim.quasi-quote.xml
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.web-server
        :iolib)

  (:shadowing-import-from :hu.dwim.syntax-sugar
                          #:define-syntax)

  (:shadow #:parent
           #:test
           #:test
           #:uri)

  (:readtable-setup
   (hu.dwim.web-server::setup-readtable)
   (hu.dwim.syntax-sugar:enable-string-quote-syntax)))

(in-package :hu.dwim.web-server.test)

(import-all-owned-symbols :hu.dwim.web-server :hu.dwim.web-server.test)
