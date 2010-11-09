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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; import all the internal symbol of WUI
  (bind ((wui-package (find-package :hu.dwim.web-server))
         (wui.test-package (find-package :hu.dwim.web-server.test)))
    (iter (for symbol :in-package wui-package :external-only nil)
          (when (and (eq (symbol-package symbol) wui-package)
                     (not (find-symbol (symbol-name symbol) wui.test-package)))
            (import symbol)))))
