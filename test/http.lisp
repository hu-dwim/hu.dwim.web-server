;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2014 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def test http/x-forwarded-for ()
  (is (iolib.sockets:ipv4-address-p (parse-x-forwarded-for-value "1.2.3.4")))
  (is (iolib.sockets:ipv6-address-p (parse-x-forwarded-for-value "2001:0db8:85a3:0000:0000:8a2e:0370:7334")))
  (bind (((:values client proxies) (parse-x-forwarded-for-value "1.2.3.4, 5.6.7.8, 7.8.9.10")))
    (is (iolib.sockets:ipv4-address-p client))
    (is (= 2 (length proxies)))
    (is (every 'iolib.sockets:ipv4-address-p proxies)))
  (bind (((:values client proxies) (parse-x-forwarded-for-value "2001:0db8:85a3:0000:0000:8a2e:0370:7334, 5.6.7.8")))
    (is (iolib.sockets:ipv6-address-p client))
    (is (= 1 (length proxies)))
    (is (every 'iolib.sockets:ipv4-address-p proxies))))
