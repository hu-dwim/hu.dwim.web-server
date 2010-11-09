;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Computation, delay and force

(def class* computation ()
  ()
  (:metaclass funcallable-standard-class))

(def (macro e) delay (&body forms)
  (with-unique-names (computation)
    `(delay* (:class-name computation :variable-name ,computation)
       ,@forms)))

(def (macro e) delay* ((&key class-name variable-name) &body forms)
  (if (and (length= 1 forms)
           (not (consp (first forms)))
           (constantp (first forms)))
      (first forms)
      `(bind ((,variable-name (make-instance ',class-name)))
         (set-funcallable-instance-function ,variable-name (named-lambda delay-body () ,@forms))
         ,variable-name)))

(def (function e) force (value)
  (if (typep value 'computation)
      (funcall value)
      value))

(def (function e) computation? (thing)
  (typep thing 'computation))

(def class* one-time-computation (computation)
  ((value :type t))
  (:metaclass funcallable-standard-class))

(def (macro e) one-time-delay (&body forms)
  (with-unique-names (computation)
    `(delay* (:class-name one-time-computation :variable-name ,computation)
       (if (slot-boundp ,computation 'value)
           (value-of ,computation)
           (setf (value-of ,computation)
                 (progn
                   ,@forms))))))

;;;;;;
;;; Utils

(def function string-to-lisp-boolean (value)
  (eswitch (value :test #'string=)
    ("true" #t)
    ("false" #f)))

(def function string-to-lisp-integer (value)
  (eswitch (value :test #'string=)
    ("true" 1)
    ("false" 0)))

(def function instance-class-name-as-string (instance)
  (class-name-as-string (class-of instance)))

(def function class-name-as-string (class)
  (string-downcase (symbol-name (class-name class))))

(def (function io) maybe-make-xml-attribute (name value)
  (when value
    (make-xml-attribute name value)))

(def function mandatory-argument ()
  (error "A mandatory argument was not specified"))

(def function function-name (function)
  (etypecase function
    (generic-function (generic-function-name function))
    (function (sb-impl::%fun-name function))))

(def function html-text? (instance)
  (declare (ignore instance))
  #t)

(def (type e) html-text (&optional maximum-length)
  "Formatted text that may contain various fonts, styles and colors as in XHTML."
  (declare (ignore maximum-length))
  `(and string
        (satisfies html-text?)))

(def function password? (instance)
  (declare (ignore instance))
  #t)

(def (type e) password ()
  `(and string
        (satisfies password?)))

(def (function i) class-prototype (class)
  (cond
    ;; KLUDGE: SBCL's class prototypes for built in classes are wrong in some cases
    ((subtypep class 'float)
     42.0)
    ((subtypep class 'string)
     "42")
    ((subtypep class 'null)
     nil)
    ((subtypep class 'list)
     '(42))
    ((subtypep class 'array)
     #(42))
    ((subtypep class 'sequence)
     '(42))
    ((subtypep class 'function)
     (lambda ()))
    (t (aprog1 (closer-mop:class-prototype (ensure-finalized class))
         (assert (or (subtypep class 'number)
                     (not (eql it 42))))))))

(def (function i) class-slots (class)
  (closer-mop:class-slots (ensure-finalized class)))

(def (function i) class-precedence-list (class)
  (closer-mop:class-precedence-list (ensure-finalized class)))

(def function remove-undefined-class-slot-initargs (class args)
  (iter (for (arg value) :on args :by 'cddr)
        (when (find arg (class-slots class) :key 'slot-definition-initargs :test 'member)
          (collect arg)
          (collect value))))

(def function trim-suffix (suffix sequence)
  (subseq sequence 0 (- (length sequence) (length suffix))))

(def function filter-slots (names slots)
  (when names
    (collect-if (lambda (slot)
                  (member (slot-definition-name slot) names))
                slots)))

(def function remove-slots (names slots)
  (if names
      (remove-if (lambda (slot)
                   (member (slot-definition-name slot) names))
                 slots)
      slots))

(deftype simple-ub8-vector (&optional (length '*))
  `(simple-array (unsigned-byte 8) (,length)))

(def (function i) coerce-to-simple-ub8-vector (vector &optional (length (length vector)))
  (if (and (typep vector 'simple-ub8-vector)
           (= length (length vector)))
      vector
      (aprog1
          (make-array length :element-type '(unsigned-byte 8))
        (replace it vector :end2 length))))

(def function assert-file-exists (file)
  (assert (cl-fad:file-exists-p file))
  file)

(def constant +timestamp-parameter-name+ "_ts")

(def function append-timestamp-to-uri (uri timestamp)
  (labels ((to-timestamp-string (thing)
             (etypecase thing
               (integer (integer-to-string (mod thing 10000)))
               (local-time:timestamp (to-timestamp-string (local-time:sec-of thing))))))
    (setf (uri-query-parameter-value uri +timestamp-parameter-name+)
          (to-timestamp-string (force timestamp))))
  uri)

(def function single-argument-layered-method-definer (name forms &key default-layer options whole)
  (when (or (getf options :in)
            (getf options :in-layer))
    (warn "Most probably there's a misplaced layer definition in a ~S; the form is ~S" name whole))
  (bind ((layer (if (member (first forms) '(:in-layer :in))
                    (progn
                      (pop forms)
                      (pop forms))
                    default-layer))
         (qualifier (when (or (keywordp (first forms))
                              (member (first forms) '(and or progn append nconc)))
                      (pop forms)))
         (type (pop forms)))
    `(def (layered-method ,@options) ,name ,@(when layer `(:in ,layer)) ,@(when qualifier (list qualifier)) ((-self- ,type))
          ,@forms)))

(def (function i) us-ascii-octets-to-string (vector)
  (coerce (babel:octets-to-string vector :encoding :us-ascii) 'simple-base-string))
(def (function i) string-to-us-ascii-octets (string)
  (babel:string-to-octets string :encoding :us-ascii))

(def (function i) iso-8859-1-octets-to-string (vector)
  (babel:octets-to-string vector :encoding :iso-8859-1))
(def (function i) string-to-iso-8859-1-octets (string)
  (babel:string-to-octets string :encoding :iso-8859-1))

(def (function i) utf-8-octets-to-string (vector)
  (babel:octets-to-string vector :encoding :utf-8))
(def (function i) string-to-utf-8-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(def generic encoding-name-of (thing)
  (:method ((self external-format))
    (encoding-name-of (external-format-encoding self)))

  (:method ((self babel-encodings:character-encoding))
    (babel-encodings:enc-name self)))

(def function get-bytes-allocated ()
  "Returns a monotonic counter of bytes allocated, preferable a per-thread value."
  #*((:sbcl
      ;; as of 1.0.22 this is still a global value shared by all threads
      (sb-ext:get-bytes-consed))
     (t
      0)))

(def (function io) shrink-vector (vector size)
  "Fast shrinking for simple vectors. It's not thread-safe, use only on local vectors!"
  #+allegro
  (excl::.primcall 'sys::shrink-svector vector size)
  #+sbcl
  (setq vector (sb-kernel:%shrink-vector vector size))
  #+cmu
  (lisp::shrink-vector vector size)
  #+lispworks
  (system::shrink-vector$vector vector size)
  #+scl
  (common-lisp::shrink-vector vector size)
  #-(or allegro cmu lispworks sbcl scl)
  (setq vector (subseq vector 0 size))
  vector)

(def function owner-class-of-effective-slot-definition (effective-slot)
  "Returns the class to which the given slot belongs."
  #+sbcl(slot-value effective-slot 'sb-pcl::%class)
  #-sbcl(not-yet-implemented))

(def (function io) make-adjustable-vector (initial-length &key (element-type t))
  (declare (type array-index initial-length))
  (make-array initial-length :adjustable #t :fill-pointer 0 :element-type element-type))

(def (function i) make-displaced-array (array &optional (start 0) (end (length array)))
  (make-array (- end start)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset start))

(def (function o) split-ub8-vector (separator line)
  (declare (type simple-ub8-vector line)
           (inline make-displaced-array))
  (iter outer ; we only need the outer to be able to collect a last chunk in the finally block of the inner loop
        (iter (with start = 0)
              (for end :upfrom 0)
              (for char :in-vector line)
              (declare (type fixnum start end))
              (when (= separator char)
                (in outer (collect (make-displaced-array line start end)))
                (setf start (1+ end)))
              (finally (in outer (collect (make-displaced-array line start)))))
        (while nil)))

(def (function i) is-lock-held? (lock)
  #*((:sbcl (debug-only (check-type lock sb-thread:mutex))
            (eq (sb-thread:mutex-owner lock) sb-thread:*current-thread*))
     (t #.(warn "~S is not implemented for your platform." 'is-lock-held?)
        (error "~S is not implemented for your platform." 'is-lock-held?))))

(def function mailto-href (email-address)
  (string+ "mailto:" email-address))

(def (function io) new-random-hash-table-key (hash-table key-length &key prefix)
  (iter (for key = (random-string key-length +ascii-alphabet+ prefix))
        (for (values value foundp) = (gethash key hash-table))
        (when (not foundp)
          (return key))))

(def (function io) insert-with-new-random-hash-table-key (hash-table value key-length &key prefix)
  (bind ((key (new-random-hash-table-key hash-table key-length :prefix prefix)))
    (setf (gethash key hash-table) value)
    (values key value)))

;;;;;;
;;; Debug on error

(def class* debug-context-mixin ()
  ((debug-on-error :type boolean :accessor nil)))

(def method debug-on-error? ((context debug-context-mixin) error)
  (if (slot-boundp context 'debug-on-error)
      (slot-value context 'debug-on-error)
      (call-next-method)))

;;;;;;
;;; Tree

(def (function o) find-ancestor (node parent-function predicate &key (otherwise nil otherwise?))
  (ensure-functionf parent-function predicate)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (while current-node)
        (when (funcall predicate current-node)
          (return current-node))
        (finally (return (handle-otherwise (error "Could not find ancestor component using visitor ~A starting from ~A using parent-function ~A"
                                                  predicate node parent-function))))))

(def (function o) find-root (node parent-function)
  (ensure-functionf parent-function)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (for previous-node :previous current-node)
        (while current-node)
        (finally (return previous-node))))

(def (function o) map-parent-chain (node parent-function map-function)
  (ensure-functionf parent-function map-function)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (while current-node)
        (funcall map-function current-node)))

(def (function o) map-tree (node children-function map-function)
  (ensure-functionf children-function map-function)
  (map-tree* node children-function
             (lambda (node parent level)
               (declare (ignore parent level))
               (funcall map-function node))))

(def (function o) map-tree* (node children-function map-function &optional (level 0) parent)
  (declare (type fixnum level))
  (ensure-functionf children-function map-function)
  (cons (funcall map-function node parent level)
        (map 'list (lambda (child)
                     (map-tree* child children-function map-function (1+ level) node))
             (funcall children-function node))))

;;;;;;
;;; Dynamic classes

(def special-variable *dynamic-classes* (make-hash-table :test #'equal))

(def function find-dynamic-class (class-names)
  (assert (every #'symbolp class-names))
  (gethash class-names *dynamic-classes*))

(def function (setf find-dynamic-class) (class class-names)
  (assert (every #'symbolp class-names))
  (setf (gethash class-names *dynamic-classes*) class))

(def function dynamic-class-name (class-names)
  (format-symbol *package* "~{~A~^&~}" class-names))

(def function dynamic-class-metaclass (class-names)
  (bind ((metaclasses
          (mapcar (lambda (class-name)
                    (class-of (find-class class-name)))
                  class-names)))
    (every (lambda (metaclass)
             (eq metaclass (first metaclasses)))
           metaclasses)
    (first metaclasses)))

(def function ensure-dynamic-class (class-names)
  (ensure-class (dynamic-class-name class-names) :direct-superclasses class-names :metaclass (dynamic-class-metaclass class-names)))

(def method make-instance ((class-names list) &rest args)
  (bind ((class (find-dynamic-class class-names)))
    (unless class
      (setf class (ensure-dynamic-class class-names))
      (setf (find-dynamic-class class-names) class))
    (apply #'make-instance class args)))

(def macro ensure-instance (place type &rest args &key &allow-other-keys)
  `(aif ,place
        (reinitialize-instance it ,@args)
        (setf ,place (make-instance ,type ,@args))))

;;;;;
;;; Hash set

(def function make-hash-set-from-list (elements &key (test #'eq) (key #'identity))
  (ensure-functionf key test)
  (prog1-bind set
      (make-hash-table :test test)
    (dolist (element elements)
      (setf (gethash (funcall key element) set) element))))
