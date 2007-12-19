(in-package :alexandria)

(declaim (inline ensure-symbol))
(defun ensure-symbol (name &optional (package *package*))
  "Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of INTERN.

Example: (ENSURE-SYMBOL :CONS :CL) => CL:CONS, :EXTERNAL"
  (intern (string name) package))

(defun make-formatted-symbol (package name)
  (case package
    ((nil)
     (make-symbol name))
    ((t)
     (intern name))
    (t
     (intern name package))))

(declaim (inline format-symbol))
(defun format-symbol (package control &rest arguments)
  "Constructs a string by applying ARGUMENTS to CONTROL as if by FORMAT, and
then creates a symbol named by that string. If PACKAGE is NIL, returns an
uninterned symbol, if package is T, returns a symbol interned in the current
package, and otherwise returns a symbol interned in the package designated by
PACKAGE."
  (values
   (make-formatted-symbol package (apply #'format nil control arguments))))

(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (intern (string name) :keyword))

(defun make-gensym-list (length &optional (x "G"))
  "Returns a list of LENGTH gensyms, each generated with a call to
GENSYM using (if provided) as the argument."
  (loop repeat length
        collect (gensym x)))
