(in-package :alexandria)

(declaim (inline clamp))
(defun clamp (number min max)
  "Clamps the NUMBER into [MIN, MAX] range. Returns MIN if NUMBER lesser then
MIN and MAX if NUMBER is greater then MAX, otherwise returns NUMBER."
  (if (< number min)
      min
      (if (> number max)
          max
          number)))

(defun gaussian-random (&optional min max)
  "Returns two gaussian random double floats as the primary and secondary value,
optionally constrained by MIN and MAX. Gaussian random numbers form a standard
normal distribution around 0.0d0."
  (labels ((gauss () 
             (loop 
                for x1 = (- (random 2.0d0) 1.0d0)
                for x2 = (- (random 2.0d0) 1.0d0)
                for w = (+ (expt x1 2) (expt x2 2))
                when (< w 1.0d0)
                do (let ((v (sqrt (/ (* -2.0d0 (log w)) w))))
                     (return (values (* x1 v) (* x2 v))))))
           (guard (x min max)
             (unless (<= min x max)
               (tagbody
                :retry
                  (multiple-value-bind (x1 x2) (gauss)
                    (when (<= min x1 max)
                      (setf x x1)
                      (go :done))
                    (when (<= min x2 max)
                      (setf x x2)
                      (go :done))
                    (go :retry))
                :done))
             x))
    (multiple-value-bind (g1 g2) (gauss)
      (values (guard g1 (or min g1) (or max g1))
              (guard g2 (or min g2) (or max g2))))))

(declaim (inline iota))
(defun iota (n &key (start 0) (step 1))
  "Return a list of n numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 0.

Examples:

  (iota 4)                      => (0 1 2 3 4)
  (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
  (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)
"
  (declare (type (integer 0) n) (number start step))
  (loop repeat n
     ;; KLUDGE: get numeric contagion right for the first element too
     for i = (+ start (- step step)) then (+ i step)
     collect i))

(declaim (inline lerp))
(defun lerp (v a b)
  "Returns the result of linear interpolation between A and B, using the
interpolation coefficient V."
   (+ a (* v (- b a))))

(declaim (inline mean))
(defun mean (sample)
  "Returns the mean of SAMPLE. SAMPLE must be a sequence of numbers."
  (/ (reduce #'+ sample) (length sample)))

(declaim (inline median))
(defun median (sample)
  "Returns median of SAMPLE. SAMPLE must be a sequence of real numbers."
  (let* ((vector (sort (copy-sequence 'vector sample) #'<))
         (length (length vector))
         (middle (truncate length 2)))
    (if (oddp length)
        (aref vector middle)
        (/ (+ (aref vector middle) (aref vector (1+ middle))) 2))))

(declaim (inline variance))
(defun variance (sample &key (biased t))
  "Variance of SAMPLE. Returns the biased variance if BIASED is true (the default),
and the unbiased estimator of variance if BIASED is false. SAMPLE must be a
sequence of numbers."
  (let ((mean (mean sample)))
    (/ (reduce (lambda (a b)
                 (+ a (expt (- b mean) 2)))
               sample
               :initial-value 0)
       (- (length sample) (if biased 0 1)))))

(declaim (inline standard-deviation))
(defun standard-deviation (sample &key (biased t))
  "Standard deviation of SAMPLE. Returns the biased standard deviation if
BIASED is true (the default), and the square root of the unbiased estimator
for variance if BIASED is false (which is not the same as the unbiased
estimator for standard deviation). SAMPLE must be a sequence of numbers."
  (sqrt (variance sample :biased biased)))

(define-modify-macro maxf (&rest numbers) max
  "Modify-macro for MAX. Sets place designated by the first argument to the
maximum of its original value and NUMBERS.")

(define-modify-macro minf (&rest numbers) min
  "Modify-macro for MIN. Sets place designated by the first argument to the
minimum of its original value and NUMBERS.")
