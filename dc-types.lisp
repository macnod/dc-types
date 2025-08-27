(in-package :dc-types)

;; BEGIN list-of
(defparameter *list-of-cache* (make-hash-table :test #'equal))

(defun make-list-of-predicate (element-type)
  (lambda (object)
    (loop for item in object
          always (typep item element-type))))

(deftype list-of (element-type)
  "Defines a type that describes a list of elements of type ELEMENT-TYPE."
  (let ((predicate-name (gethash element-type *list-of-cache*)))
    (unless predicate-name
      (setq predicate-name 
        (intern (format nil "LIST-OF-~S-P" element-type)))
      (setf (symbol-function predicate-name) 
        (make-list-of-predicate element-type))
      (setf (gethash element-type *list-of-cache*) 
        predicate-name))
    `(satisfies ,predicate-name)))
;; END list-of

;; BEGIN hash-table-of
(defparameter *hash-table-of-cache* (make-hash-table :test 'equal))

(defun make-hash-table-of-predicate (key-type value-type)
  (lambda (object)
    (if (null object)
      t
      (loop for key being the hash-keys in object using (hash-value value)
        always (and 
                 (typep key key-type)
                 (typep value value-type))))))

(deftype hash-table-of (key-type value-type)
  "Defines a type that describes a hash table with keys of type KEY-TYPE and values
of type VALUE-TYPE."
  (let ((predicate-name (gethash (list key-type value-type) *hash-table-of-cache*)))
    (unless predicate-name
      (setq predicate-name
        (intern
          (substitute-if 
            #\-
            (complement #'alphanumericp) 
            (format nil "HASH-TABLE-OF-~S-~S-P" key-type value-type))))
      (setf (symbol-function predicate-name) (make-hash-table-of-predicate key-type value-type))
      (setf (gethash (list key-type value-type) *hash-table-of-cache*) predicate-name))
    `(satisfies ,predicate-name)))
;; END hash-table-of

(deftype vector-of (element-type)
  "Defines a type for a vector where all elements are of ELEMENT-TYPE."
  `(or null (vector ,element-type *)))
