(in-package :alexandria)

(defmacro define-constant (name initial-value &key (test 'eql) documentation)
  "Ensures that the global variable named by NAME is a constant with a value
that is equal under TEST to the result of evaluating INITIAL-VALUE. TEST
defaults to EQL, and if given it must be a symbol naming a function. If
DOCUMENTATION is given, it becomes the documentation string of the constant.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is not
equal under TEST to result of evaluating INITIAL-VALUE."
  `(defconstant ,name
     (let ((new ,initial-value))
       (if (boundp ',name)
           (let ((old (symbol-value ',name)))
             (cond
               ((constantp ',name)
                (cond
                  ((,test old new)
                   old)
                  (t
                   (cerror "Try to redefine the constant."
                           "~@<~S is an already defined constant whose value ~
                            ~S is not equal to the provided initial value ~S ~
                            under ~S.~:@>" ',name old new ',test)
                   new)))
               (t
                (cerror "Try to redefine the variable as a constant."
                        "~@<~S is an already bound non-constant variable ~
                         whose value is ~S.~:@>" ',name old)
                new)))
           new))
     ,@(when documentation `(,documentation))))
