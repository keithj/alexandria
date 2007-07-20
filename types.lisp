(in-package :alexandria)

(defun of-type (type)
  "Returns a function of one argument, which returns true when its argument is
of TYPE."
  (lambda (thing) (typep thing type)))

(define-compiler-macro of-type (&whole form type &environment env)
  ;; This can yeild a big benefit, but no point inlining the function
  ;; all over the place if TYPE is not constant.
  (if (constantp type env)
      (with-gensyms (thing)
        `(lambda (,thing)
           (typep ,thing ,type)))
      form))

(declaim (inline type=))
(defun type= (type1 type2)
  "Returns a primary value of T is TYPE1 and TYPE2 are the same type,
and a secondary value that is true is the type equality could be reliably
determined: primary value of NIL and secondary value of T indicates that the
types are not equivalent."
  (multiple-value-bind (sub ok) (subtypep type1 type2)
    (cond ((and ok sub)
           (subtypep type2 type1))
          (ok
           (values nil ok))
          (t
           (multiple-value-bind (sub ok) (subtypep type2 type1)
             (declare (ignore sub))
             (values nil ok))))))

(define-modify-macro coercef (type-spec) coerce
  "Modify-macro for COERCE.")
