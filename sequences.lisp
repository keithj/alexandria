(in-package :alexandria)

(defun rotate-tail-to-head (sequence n)
  (declare (type (integer 1) n))
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

(defun rotate-head-to-tail (sequence n)
  (declare (type (integer 1) n))
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

(defun rotate (sequence &optional (n 1))
  "Returns a sequence of the same type as SEQUENCE, with the elements of
SEQUENCE rotated by N: N elements are moved from the end of the sequence to
the front if N is positive, and -N elements moved from the front to the end if
N is negative. SEQUENCE must be a proper sequence. N must be an integer,
defaulting to 1. If absolute value of N is greater then the length of the
sequence, the results are identical to calling ROTATE with (* (SIGNUM N) (MOD
N (LENGTH SEQUENCE))). The original sequence may be destructively altered, and
result sequence may share structure with it."
  (if (plusp n)
      (rotate-tail-to-head sequence n)
      (if (minusp n)
          (rotate-head-to-tail sequence (- n))
          sequence)))

(defun shuffle (sequence &key (start 0) end)
  "Returns a random permutation of SEQUENCE bounded by START and END.
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
  "Type designator for proper sequences, that is proper lists and sequences
that are not lists."
  `(or proper-list
       (and (not list) sequence)))

(defun emptyp (sequence)
  "Returns true if SEQUENCE is an empty sequence. Signals an error if SEQUENCE
is not a sequence"
  (etypecase sequence
    (list (null sequence))
    (sequence (zerop (length sequence)))))

(defun sequence-of-length-p (sequence length)
  "Return true if SEQUENCE is a sequence of length LENGTH. Signals an error if
SEQUENCE is not a sequence. Returns FALSE for circular lists."
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
  ;; Can't just directly use ELT, as it is not guaranteed to signal the 
  ;; type-error.
  (cond  ((consp sequence)
          (car sequence))
         ((and (typep sequence '(and sequence (not list))) (plusp (length sequence)))
          (elt sequence 0))
         (t
          (error 'type-error 
                 :datum sequence 
                 :expected-type '(and sequence (not (satisfies emptyp)))))))

(defun (setf first-elt) (object sequence)
  "Sets the first element of SEQUENCE. Signals a type-error if SEQUENCE is
not a sequence, is an empty sequence, or if OBJECT cannot be stored in SEQUENCE."
  ;; Can't just directly use ELT, as it is not guaranteed to signal the 
  ;; type-error.
  (cond ((consp sequence)
         (setf (car sequence) object))
        ((and (typep sequence '(and sequence (not list)))
              (plusp (length sequence)))
         (setf (elt sequence 0) object))
        (t
         (error 'type-error 
                :datum sequence 
                :expected-type '(and sequence (not (satisfies emptyp)))))))

(defun last-elt (sequence)
  "Returns the last element of SEQUENCE. Signals a type-error if SEQUENCE is
not a proper sequence, or is an empty sequence."
  ;; Can't just directly use ELT, as it is not guaranteed to signal the 
  ;; type-error.
  (let ((len 0))
    (cond ((consp sequence)
           (lastcar sequence))
          ((and (typep sequence '(and sequence (not list))) (plusp (setf len (length sequence))))
           (elt sequence (1- len)))
          (t
           (error 'type-error 
                  :datum sequence 
                  :expected-type '(and proper-sequence (not (satisfies emptyp))))))))

(defun (setf last-elt) (object sequence)
  "Sets the last element of SEQUENCE. Signals a type-error if SEQUENCE is not a proper
sequence, is an empty sequence, or if OBJECT cannot be stored in SEQUENCE."
  (let ((len 0))
    (cond ((consp sequence)
           (setf (lastcar sequence) object))
          ((and (typep sequence '(and sequence (not list))) (plusp (setf len (length sequence))))
           (setf (elt sequence (1- len)) object))
          (t
           (error 'type-error 
                  :datum sequence 
                  :expected-type '(and proper-sequence (not (satisfies emptyp))))))))

(defun starts-with-subseq (sequence prefix &rest args &key (return-suffix nil) &allow-other-keys)
  "Test whether the first elements of SEQUENCE are the same (as per TEST) as the elements of PREFIX.

If RETURN-SUFFIX is T the functions returns, as a second value, a
displaced array pointing to the sequence after PREFIX."
  (remove-from-plistf args :return-suffix)
  (let ((sequence-length (length sequence))
        (prefix-length (length prefix)))
    (if (<= prefix-length sequence-length)
        (let ((mismatch (apply #'mismatch sequence prefix args)))
          (if mismatch
              (if (< mismatch prefix-length)
                  (values nil nil)
                  (values t (when return-suffix
                              (make-array (- sequence-length mismatch)
                                          :element-type (array-element-type sequence)
                                          :displaced-to sequence
                                          :displaced-index-offset prefix-length
                                          :adjustable nil))))
              (values t (when return-suffix
                          (make-array 0 :element-type (array-element-type sequence)
                                      :adjustable nil)))))
        (values nil nil))))

(defun ends-with-subseq (sequence suffix &key (test #'eql))
  "Test whether SEQUENCE ends with SUFFIX. In other words: return true if
the last (length SUFFIX) elements of SEQUENCE are equal to SUFFIX."
  (let ((sequence-length (length sequence))
        (suffix-length (length suffix)))
    (when (< sequence-length suffix-length)
      ;; if SEQUENCE is shorter than SUFFIX, then SEQUENCE can't end with SUFFIX.
      (return-from ends-with-subseq nil))
    (loop for sequence-index from (- sequence-length suffix-length) below sequence-length
          for suffix-index from 0 below suffix-length
          when (not (funcall test (elt sequence sequence-index) (elt suffix suffix-index)))
          do (return-from ends-with-subseq nil)
          finally (return t))))

(defun starts-with (object sequence &key (test #'eql) (key #'identity))
  "Returns true if SEQUENCE is a sequence whose first element is EQL to OBJECT.
Returns NIL if the SEQUENCE is not a sequence or is an empty sequence."
  (funcall test
           (funcall key 
                    (typecase sequence
                      (cons (car sequence))
                      (sequence
                       (if (plusp (length sequence))
                           (elt sequence 0)
                           (return-from starts-with nil)))
                      (t
                       (return-from starts-with nil))))
           object))

(defun ends-with (object sequence &key (test #'eql) (key #'identity))
  "Returns true if SEQUENCE is a sequence whose last element is EQL to OBJECT.
Returns NIL if the SEQUENCE is not a sequence or is an empty sequence. Signals
an error if SEQUENCE is an improper list."
  (funcall test
           (funcall key
                    (typecase sequence
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
                       (return-from ends-with nil))))
           object))
