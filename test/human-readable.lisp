;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

;;;;;;
;;; Test

(def suite (test/human-readable :in test))

(def test test/human-readable/invariant (object string)
  (is (equal string (serialize/human-readable object)))
  (is (equal object (deserialize/human-readable string))))

(def test test/human-readable/object ()
  (test/human-readable/invariant nil "")
  (test/human-readable/invariant :file "/FILE")
  (test/human-readable/invariant :class "/CLASS")
  (test/human-readable/invariant :function "/FUNCTION")
  (test/human-readable/invariant (find-class 'class) "/CLASS/COMMON-LISP:CLASS")
  (test/human-readable/invariant (find-slot 'server 'handler) "/CLASS/HU.DWIM.WEB-SERVER:SERVER/EFFECTIVE-SLOT/HU.DWIM.WEB-SERVER::HANDLER")
  (test/human-readable/invariant (fdefinition 'list) "/FUNCTION/COMMON-LISP:LIST")
  (test/human-readable/invariant (def project test-project :path (system-directory :hu.dwim.web-server)) "/PROJECT/HU.DWIM.WEB-SERVER.TEST::TEST-PROJECT")
  (test/human-readable/invariant (def book test-book ()) "/BOOK/HU.DWIM.WEB-SERVER.TEST::TEST-BOOK"))
