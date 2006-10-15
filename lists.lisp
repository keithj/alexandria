(in-package :alexandria)

(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
argument.")

(defun circular-list (&rest elements)
  "Creates a circular list of ELEMENTS."
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(defun circular-list-p (object)
  "Returns true if OBJECT is a circular list, NIL otherwise."
  (and (listp object)
       (do ((fast object (cddr fast))
            (slow (cons (car object) (cdr object)) (cdr slow)))
           (nil)
         (unless (and (consp fast) (listp (cdr fast)))
           (return nil))
         (when (eq fast slow)
           (return t)))))

(defun circular-tree-p (object)
  "Returns true if OBJECT is a circular tree, NIL otherwise."
  (labels ((circularp (object seen)
             (and (consp object)
                  (do ((fast (cons (car object) (cdr object)) (cddr fast))
                       (slow object (cdr slow)))
                      ((or (not (consp fast)) (not (consp (cdr slow))))
                       (do ((tail object (cdr tail)))
                           ((not (consp tail))
                            nil)
                         (let ((elt (car tail)))
                           (circularp elt (cons object seen)))))
                    (when (or (eq fast slow) (member slow seen))
                      (return-from circular-tree-p t))))))
    (circularp object nil)))

(defun proper-list-p (object)
  "Returns true if OBJECT is a proper list."
  (cond ((not object)
         t)
        ((consp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (listp fast) (consp (cdr fast)))
             (return (and (listp fast) (not (cdr fast)))))
           (when (eq fast slow)
             (return nil))))
        (t
         nil)))

(deftype proper-list ()
  "Type designator for proper lists. Implemented as a SATISFIES type, hence
not recommended for performance intensive use. Main usefullness as a type
designator of the expexted type in a TYPE-ERROR."
  `(satisfies proper-list-p))

(defun lastcar (list)
  "Returns the last element of LIST. Signals a type-error if LIST is not a
proper list."
  (do ((last list fast)
       (fast list (cddr fast))
       (slow (cons (car list) (cdr list)) (cdr slow)))
      (nil)
    (when (endp fast)
      (return (cadr last)))
    (when (endp (cdr fast))
      (return (car fast)))
    (when (eq fast slow) 
      (error 'type-error 
             :datum list
             :expected-type '(and list (not circular-list))))))

(defun make-circular-list (length &key initial-element)
  "Creates a circular list of LENGTH with the given INITIAL-ELEMENT."
  (let ((cycle (make-list length :initial-element initial-element)))
    (nconc cycle cycle)))

(deftype circular-list ()
  "Type designator for circular lists. Implemented as a SATISFIES type, so not
recommended for performance intensive use. Main usefullness as the
expected-type designator of a TYPE-ERROR."
  `(satisfies circular-list-p))

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

(defun remove-keys (keys plist)
  "Returns a fresh propery-list with same keys and values as PLIST, except
that keys in the list designated by KEYS and values corresponding to them are
removed."
  (do ((new nil)
       (tail plist (cddr tail)))
      ((endp tail)
       (nreverse new))
    (let ((key (car tail)))
      (unless (member key keys)
        (push key new)
        (push (cadr tail) new)))))

(defun mappend (function &rest lists)
  "Applies FUNCTION to respective element(s) of each LIST, appending all the
all the result list to a single list. FUNCTION must return a list."
  (loop for results in (apply #'mapcar function lists)
        append results))

(defun set-equal (list1 list2 &key (test #'eql) (key #'identity))
  "Returns true if every element of LIST1 matches some element of LIST2 and
every element of LIST2 matches some element of LIST1. Otherwise returns false."
  (let* ((table1 (make-hash-table :test test))
         (table2 (make-hash-table :test test))
         (keylist1 (mapcar (lambda (elt)
                             (let ((elt-key (funcall key elt)))
                               (setf (gethash elt-key table1) t)
                               elt-key))
                           list1)))
    (dolist (elt list2)
      (let ((elt-key (funcall key elt)))
        (unless (gethash elt-key table1)
          (return-from set-equal nil))
        (setf (gethash elt-key table2) t)))
    (dolist (elt-key keylist1)
      (unless (gethash elt-key table2)
        (return-from set-equal nil))))
  t)

