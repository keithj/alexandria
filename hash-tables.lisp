(in-package :alexandria)

(defun copy-hash-table (table &key 
                        (test (hash-table-test table))
                        (size (hash-table-size table))
                        (rehash-size (hash-table-size table))
                        (rehash-threshold (hash-table-rehash-threshold table)))
  "Returns a shallow copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments."
  (let ((copy (make-hash-table :test test :size size 
                               :rehash-size rehash-size 
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) v))
             table)
    copy))

(declaim (inline maphash-keys))
(defun maphash-keys (function table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           table))

(declaim (inline maphash-values))
(defun maphash-values (function table)
  "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall function v))
           table))

(defun hash-table-keys (table)
  "Returns a list containing the keys of hash table TABLE."
  (let ((keys nil))
    (maphash-keys (lambda (k)
                    (push k keys))
                  table)
    keys))

(defun hash-table-values (table)
  "Returns a list containing the values of hash table TABLE."
  (let ((values nil))
    (maphash-values (lambda (v)
                      (push v values))
                    table)
    values))

(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table
TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(defun hash-table-plist (table)
  "Returns a property list containing the keys and values of hash table
TABLE."
  (let ((plist nil))
    (maphash (lambda (k v)
               (setf plist (list* k v plist)))
             table)
    plist))

(defun alist-hash-table (alist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the association list
ALIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (dolist (cons alist)
      (setf (gethash (car cons) table) (cdr cons)))
    table))

(defun plist-hash-table (plist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (setf (gethash (car tail) table) (cadr tail)))
    table))

