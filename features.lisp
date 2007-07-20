(in-package :alexandria)

(defun featurep (feature-expression)
  "Returns T if the argument matches the state of the *FEATURES*
list and NIL if it does not. FEATURE-EXPRESSION can be any atom
or list acceptable to the reader macros #+ and #-."
  (etypecase feature-expression
    (symbol (not (null (member feature-expression *features*))))
    (cons (ecase (first feature-expression)
            (:and (every #'featurep (rest feature-expression)))
            (:or  (some #'featurep (rest feature-expression)))
            (:not (not (featurep (cadr feature-expression))))))))
