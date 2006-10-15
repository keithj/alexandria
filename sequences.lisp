(in-package :alexandria)

(defun rotate-left (sequence &optional (n 1))
  "Rotates the SEQUENCE to left by N, by moving N elements from the end of the
sequence to the front. Resulting sequence may share structure with the
original one. N defaults to 1. Sequence must be a proper sequence. N can be
creater then the length of sequence."
  (if (listp sequence)
      (let ((m (mod n (list-length sequence))))
        (if (null (cdr sequence))
            sequence
            (let* ((tail (last sequence (+ m 1)))
                   (last (cdr tail)))
              (setf (cdr tail) nil)
              (nconc last sequence))))
      (let* ((len (length sequence))
             (m (mod n len))
             (tail (subseq sequence (- len m))))
        (replace sequence sequence :start1 m :start2 0)
        (replace sequence tail)
        sequence)))

(defun rotate-right (sequence &optional (n 1))
    "Rotates the SEQUENCE to right by N, by moving N element from the front of
the sequence to the end. Resulting sequence may share structure with the
original one. N defaults to 1."
  (if (listp sequence)
      (let ((m (mod (1- n) (list-length sequence))))
        (if (null (cdr sequence))
            sequence
            (let* ((headtail (nthcdr m sequence))
                   (tail (cdr headtail)))
              (setf (cdr headtail) nil)
              (nconc tail sequence))))
      (let* ((len (length sequence))
             (m (mod n len))
             (head (subseq sequence 0 m)))
        (replace sequence sequence :start1 0 :start2 m)
        (replace sequence head :start1 (- len m))
        sequence)))

(defun suffle (sequence &key (start 0) end)
  "Returns a radom permutation of SEQUENCE bounded by START and END.
Permuted sequence may share storage with the original one. Signals
an error if SEQUENCE is not a proper sequence."
  (declare (fixnum start) ((or fixnum null) end))
  (let ((end (or end (if (listp sequence) (list-length sequence) (length sequence)))))
    (loop for i from start below end
       do (rotatef (elt sequence i) (elt sequence (random end)))))
  sequence)

(defun random-elt (sequence &key (start 0) end)
  "Returns a random element from SEQUENCE bounded by START and END. Signals an
error if the SEQUENCE is not a proper sequence."
  (declare (sequence sequence) (fixnum start) ((or fixnum null) end))
  (let ((i (+ start (random (- (or end  (if (listp sequence)
                                            (list-length sequence)
                                            (length sequence)))
                               start)))))
    (elt sequence i)))

(define-modify-macro removef (item &rest remove-keywords)
  (lambda (seq item &rest keyword-arguments)
    (apply #'remove item seq keyword-arguments))
  "Modify-macro for REMOVE. Sets place designated by the first argument to
the result of calling REMOVE with ITEM, place, and the REMOVE-KEYWORDS.")

(define-modify-macro deletef (item &rest remove-keywords)
  (lambda (seq item &rest keyword-arguments)
    (apply #'delete item seq keyword-arguments))
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")

(deftype proper-sequence ()
  `(or vector 
       proper-list
       (and (not sequence) (not list) sequence)))

(defun emptyp (sequence)
  "Returns true if SEQUENCE is an empty sequence. Signals an error if SEQUENCE
is not a sequence"
  (etypecase sequence
    (list (null sequence))
    (sequence (zerop (length sequence)))))

(defun sequence-of-length-p (sequence length)
  "Return true if SEQUENCE is a sequence of length LENGTH. Signals an error if
SEQUENCE is not a sequence."
  (etypecase sequence
    (null
     (zerop length))
    (cons
     (let ((n (1- length)))
       (unless (minusp n)
         (let ((tail (nthcdr n sequence)))
           (and tail (null (cdr tail)))))))
    (sequence
     (= length (length sequence)))))

(declaim (inline copy-sequence))
(defun copy-sequence (type sequence)
  "Returns a fresh sequence of TYPE, which has the same elements as
SEQUENCE."
  (if (typep sequence type)
      (copy-seq sequence)
      (coerce sequence type)))

(defun first-elt (sequence)
  "Returns the first element of SEQUENCE. Signals a type-error if SEQUENCE is
not a sequence, or is an empty sequence."
  (cond  ((consp sequence)
          (car sequence))
         ((and (vectorp sequence) (plusp (length sequence)))
          (aref sequence 0))
         (t
          (error 'type-error 
                 :datum sequence 
                 :expected-type '(and sequence (not (satisfies emptyp)))))))

(defun last-elt (sequence)
  "Returns the last element of SEQUENCE. Signals a type-error if SEQUENCE is
not a proper sequence, or is an empty sequence."
  (declare (inline lastcar))
  (let ((len 0))
    (cond ((consp sequence)
           (lastcar sequence))
          ((and (vectorp sequence) (plusp (setf len (length sequence))))
           (aref sequence (1- len)))
         (t
          (error 'type-error 
                 :datum sequence 
                 :expected-type '(and proper-sequence (not (satisfies emptyp))))))))

(defun starts-with (object sequence)
  "Returns true if SEQUENCE is a sequence whose first element is EQL to OBJECT.
Returns NIL if the SEQUENCE is not a sequence or is an empty sequence."
  (eql (typecase sequence
         (cons (car sequence))
         (sequence
          (if (plusp (length sequence))
              (elt sequence 0)
              (return-from starts-with nil)))
         (t
          (return-from starts-with nil)))
       object))

(defun ends-with (object sequence)
  "Returns true if SEQUENCE is a sequence whose last element is EQL to OBJECT.
Returns NIL if the SEQUENCE is not a sequence or is an empty sequence. Signals
an error if SEQUENCE is an improper list."
  (eql (typecase sequence
         (cons 
          ;; signals for improper lists
          (lastcar sequence)) 
         (sequence
          ;; Can't use last-elt, as that signals an error for empty sequences          
          (let ((len (length sequence)))
            (if (plusp len)
                (elt sequence (1- len))
                (return-from ends-with nil))))
         (t
          (return-from ends-with nil)))
       object))
