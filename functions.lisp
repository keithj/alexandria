(in-package :alexandria)

(defun disjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning the primary value of the first
predicate that returns true, without calling the remaining predicates.
If none of the predicates returns true, NIL is returned."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lambda (&rest arguments)
    (or (apply predicate arguments)
        (some (lambda (p)
                (apply p arguments))
              more-predicates))))

(defun conjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning NIL if any of the predicates
returns false, without calling the remaining predicated. If none of the
predicates returns false, returns the primary value of the last predicate."
  (lambda (&rest arguments)
    (and (apply predicate arguments)
         (do ((tail (cdr more-predicates) (cdr tail))
              (head (car more-predicates) (car tail)))
             ((not tail)
              (apply head arguments))
           (unless (apply head arguments)
             (return nil))))))

(defun compose (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
            (lambda (&rest arguments)
              (declare (dynamic-extent arguments))
              (funcall f (apply g arguments))))
          more-functions
          :initial-value function))

(define-compiler-macro compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun multiple-value-compose (function &rest more-functions)
    "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies
its arguments to to each in turn, starting from the rightmost of
MORE-FUNCTIONS, and then calling the next one with all the return values of
the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (reduce (lambda (f g)
            (lambda (&rest arguments)
              (declare (dynamic-extent arguments))
              (multiple-value-call f (apply g arguments))))
          more-functions
          :initial-value function))

(define-compiler-macro multiple-value-compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(multiple-value-call ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "MV-COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

(defun curry (function &rest arguments)
  "Returns a function that applies ARGUMENTS and the arguments
it is called with to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lambda (&rest more)
    (declare (dynamic-extent more))                 
    (multiple-value-call function (values-list arguments) (values-list more))))

(define-compiler-macro curry (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "CURRY")))
    `(let ,(mapcar #'list curries arguments)
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (lambda (&rest more)
         (declare (dynamic-extent more))
         (apply ,function ,@curries more)))))

(defun rcurry (function &rest arguments)
  "Returns a function that applies the arguments it is called
with and ARGUMENTS to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (lambda (&rest more)
    (declare (dynamic-extent more))                 
    (multiple-value-call function (values-list more) (values-list arguments))))
