;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;; TODO this all should really be in local-time, or at least in an integration file with local-time

;; TODO cl-irregsexp is 6 times faster than cl-ppcre in this
#+nil
(cl-irregsexp:match-bind (weekday ", " (day (integer :length 2)) " " month " " (year (integer :length 4)) " "
                          (hour (integer :length 2)) ":" (minute (integer :length 2)) ":" (second (integer :length 2)) " GMT")
    "Sun, 06 Nov 1994 08:49:37 GMT"
  (foo weekday day month year hour minute second))

(def function %parse-timestring/construct-timestamp (weekday second minute hour day month year &key (timezone local-time:+utc-zone+))
  (bind ((timestamp (local-time:encode-timestamp 0 second minute hour day month year :timezone timezone)))
    (debug-only
      (local-time:with-decoded-timestamp (:day-of-week result-weekday :timezone timezone) timestamp
        (assert (equal weekday result-weekday))))
    timestamp))

(def constant +rfc1123-regexp+ "^(\\w{3}), (\\d{2}) (\\w{3}) (\\d{4}) (\\d{2}):(\\d{2}):(\\d{2}) ([+-]\\d{4}|GMT|UTC)$")
(def special-variable *rfc1123-scanner* (cl-ppcre:create-scanner +rfc1123-regexp+))

(def (function o) parse-rfc1123-timestring (string &key (otherwise :error))
  (macrolet ((to-integer (&rest vars)
               `(progn
                  ,@(iter (for var :in vars)
                          (collect `(setf ,var (or (ignore-errors (parse-integer ,var))
                                                   (fail)))))))
             (lookup (var values)
               `(unless (setf ,var (position ,var ,values :test #'equalp))
                  (fail)))
             (to-timezone (var)
               `(setf ,var (or (ignore-errors (%parse-timezone ,var))
                               (fail))))
             (fail ()
               `(return-from parse-rfc1123-timestring
                  (handle-otherwise/value otherwise :default-message `("Unable to parse ~S as a rfc1123 timestring" ,string)))))
    (cl-ppcre:do-register-groups (weekday day month year hour minute second timezone)
        (*rfc1123-scanner* string (fail))
      (to-integer day year hour minute second)
      (lookup month local-time:+short-month-names+)
      (lookup weekday local-time:+short-day-names+)
      (to-timezone timezone)
      (return (%parse-timestring/construct-timestamp weekday second minute hour day month year
                                                     :timezone timezone)))))

(def function %parse-timezone (tz/str)
  (cond ((member tz/str '("GMT" "UTC") :test 'string=)
         local-time::+utc-zone+)
        (t
         (bind ((sign (ecase (char tz/str 0)
                        ((#\+) +1)
                        ((#\-) -1)))
                (hh (parse-integer (subseq tz/str 1 3)))
                (mm (parse-integer (subseq tz/str 3 5)))
                (offset (* sign (+ (* hh 3600) (* mm 60)))))
           (assert (<= 0 mm 59))
           (assert (<= 0 hh 14))
           (assert (<= -43200 offset 50400))
           (local-time::%make-simple-timezone tz/str tz/str offset)))))

(def (function io) parse-http-timestring (string &key (otherwise :error otherwise?))
  (or (when (member (length string) '(29 31))
        (parse-rfc1123-timestring string :otherwise #f))
      ;; Examples:
      ;; Wed, 12 Dec 2018 12:50:22 +0100
      ;; Sun, 21 Jun 2015 01:31:02 UTC
      ;; Sun, 21 Jun 2015 01:31:02 GMT
      ;; TODO according to http://www.ietf.org/rfc/rfc1945.txt we should understand all of these:
      ;; Sun, 06 Nov 1994 08:49:37 GMT    ; RFC 822, updated by RFC 1123
      ;; Sunday, 06-Nov-94 08:49:37 GMT   ; RFC 850, obsoleted by RFC 1036
      ;; Sun Nov  6 08:49:37 1994         ; ANSI C's asctime() format
      (handle-otherwise
        (error "Unable to parse ~S as a http timestring" string))))
