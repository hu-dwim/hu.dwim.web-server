;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

(def (function o) parse-accept-language-header-value (header-value)
  (check-type header-value string)
  (bind ((*print-pretty* #f)
         (result (parse-accept-header-value header-value)))
    (labels ((convert-to-canonical-locale-name (key)
               (bind ((language nil)
                      (territory nil))
                 (iter (for char :in-vector key)
                       (for index :upfrom 0)
                       (when (char= char #\-)
                         (setf language (subseq key 0 index))
                         (setf territory (subseq key (1+ index)))
                         (return)))
                 (if territory
                     (with-output-to-string (*standard-output* nil :element-type 'base-char)
                       (write-string language)
                       (write-char #\_)
                       (write-string (string-upcase territory)))
                     key))))
      (iter (for entry :in result)
            (setf (car entry) (convert-to-canonical-locale-name (car entry)))))
    result))

(def (function o) parse-accept-header-value (header-value)
  (check-type header-value string)
  (http.dribble "Parsing Accept header ~S" header-value)
  (bind ((*print-pretty* #f)
         (index 0)
         (length (length header-value))
         (entries ())
         (key)
         (score))
    (declare (type array-index index))
    (labels ((fail ()
               (error "Failed to parse accept header value ~S" header-value))
             (make-string-buffer ()
               (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
             (next-char ()
               (when (< index length)
                 (aref header-value index)))
             (read-next-char ()
               (bind ((result (next-char)))
                 (incf index)
                 (if (and result
                          (member result '(#\Space #\Tab #\Newline #\Linefeed) :test #'char=))
                     (read-next-char)
                     result)))
             (parse-key ()
               (setf score nil)
               (setf key nil)
               (iter (for char = (read-next-char))
                     (case char
                       (#\; (parse-score))
                       (#\, (emit-entry))
                       ((nil) (emit-entry)
                              (emit-result))
                       (t
                        (unless key
                          (setf key (make-string-buffer)))
                        (vector-push-extend char key)))))
             (parse-score ()
               (unless (char= #\q (read-next-char))
                 (fail))
               (unless (char= #\= (read-next-char))
                 (fail))
               (setf score (make-string-buffer))
               (iter (for char = (read-next-char))
                     (if (and char
                              (or (alphanumericp char)
                                  (char= char #\.)))
                         (vector-push-extend char score)
                         (case char
                           (#\,
                            (emit-entry))
                           ((nil)
                            (emit-entry)
                            (emit-result))
                           (t
                            (fail))))))
             (emit-entry ()
               ;; (break "emitting ~S" (cons key score))
               (when key
                 (push (cons key
                             (if score
                                 (parse-number:parse-number score)
                                 1))
                       entries))
               (when (next-char)
                 (parse-key)))
             (emit-result ()
               (return-from parse-accept-header-value
                 (sort entries #'> :key #'cdr))))
      (parse-key))))
