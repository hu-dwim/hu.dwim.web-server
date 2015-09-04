;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2014 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server.test)

(def suite (integ/file-serving :in integration))

(def (function i) search-in-header (header text headers)
  (search text (header-alist-value headers header)))

(def macro req ((path &key (method :get)) &body macrobody)
  `(bind (((:values body status headers uri stream close-stream? reason) (drakma:http-request (format nil "http://~A:~A/data~A" *test-host* *test-port* ,path) :method ,method)))
     (declare (ignorable body status headers uri stream close-stream? reason))
     (progn
       ,@macrobody)))

(def test integ/file-serving/text-file ()
  (req ("/test.txt")
    (is (search "The quick brown fox jumps over (↷) the lazy dog." body))
    (is (search-in-header :content-type "text/plain" headers))
    (is (search-in-header :content-type "utf-8" headers))
    (is (search-in-header :cache-control "max-age=43200" headers))))

(def test integ/file-serving/dir-index ()
  (req ("/")
    (is (eq 404 status)))
  (req ("")
    (is (eq 404 status)))
  (req ("/no-dir-index")
    (is (eq 404 status)))
  (req ("/no-dir-index/")
    (is (eq 404 status)))
  (req ("/auto-dir-index/")
    (is (eq 200 status))
    (is (search "this-directory-should-be-listed" body)))
  (req ("/auto-dir-index")
    (is (eq 200 status))))

(def test integ/file-serving/cache ()
  (flet ((find-entry ()
           (loop for k being the hash-keys in (cache-of (find-broker-for-path '("data"))) using (hash-value v)
                 do (when (search "data/test.txt" (first k))
                      (return v)))))
    (bind (first-entry
           initial-write-date
           second-entry)
      (req ("/test.txt")
        (setf first-entry (find-entry))
        (setf initial-write-date (file-write-date-of first-entry)))
      (update-mtime (iolib.pathnames:file-path-namestring (file-path-of first-entry)))
      (req ("/test.txt")
        (setf second-entry (find-entry)))
      (is (< initial-write-date (file-write-date-of second-entry))))))

(defun find-broker-for-path (path)
  (dolist (b (brokers-of *test-server*))
    (typecase b
      (broker-at-path (when (equal path (path-of b))
                        (return b))))))

(defun update-mtime (file)
  (let* ((stat (iolib.syscalls:stat file)))
    (sb-posix:utime file
                    (sb-posix:stat-atime stat)
                    (sb-posix:time))))
