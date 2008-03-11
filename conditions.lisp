(in-package :alexandria)

(defun required-argument (&optional name)
  "Signals an error for a missing argument of NAME. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  (error "Required argument ~@[~S ~]missing." name))

(define-condition simple-style-warning (style-warning simple-warning)
  ())

(defun simple-style-warning (message &rest args)
  (warn 'simple-style-warning :format-control message :format-arguments args))

(defmacro ignore-some-conditions ((&rest conditions) &body body)
  "Similar to CL:IGNORE-ERRORS but the (unevaluated) CONDITIONS
list determines which specific conditions are to be ignored."
  `(handler-case
       (progn ,@body)
     ,@(loop for condition in conditions collect
             `(,condition (c) (values nil c)))))

(defmacro unwind-protect-case ((&optional abort-flag) protected-form &body clauses)
  "Like CL:UNWIND-PROTECT, but you can specify the circumstances that
the cleanup CLAUSES are run.

ABORT-FLAG is the name of a variable that will be bound to T in
CLAUSES if the PROTECTED-FORM aborted preemptively, and to NIL
otherwise.

Examples:

  (unwind-protect-case ()
       (protected-form)
     (:normal (format t \"This is only evaluated if PROTECTED-FORM executed normally.~%\"))
     (:abort  (format t \"This is only evaluated if PROTECTED-FORM aborted preemptively.~%\"))
     (:always (format t \"This is evaluated in either case.~%\")))

  (unwind-protect-case (aborted-p)
       (protected-form)
     (:always (perform-cleanup-if aborted-p)))
"
  (check-type abort-flag (or null symbol))
  (let ((gflag (gensym "FLAG+")))
    `(let ((,gflag t))
       (unwind-protect (multiple-value-prog1 ,protected-form (setf ,gflag nil))
	 (let ,(and abort-flag `((,abort-flag ,gflag)))
	   ,@(loop for (cleanup-kind . forms) in clauses
		   collect (ecase cleanup-kind
			     (:normal `(when (not ,gflag) ,@forms))
			     (:abort  `(when ,gflag ,@forms))
			     (:always `(progn ,@forms)))))))))