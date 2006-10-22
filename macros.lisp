(in-package :alexandria)

(defmacro with-unique-names (names &body forms)
  "Binds each variable named by NAMES to a unique symbol."
  `(let ,(mapcar (lambda (name)
                   `(,name (gensym ,(symbol-name name))))
                 names)
     ,@forms))

(defmacro once-only (names &body forms)
  "Evaluates FORMS with NAMES rebound to temporary variables,
ensuring that each is evaluated only once.

Example:
  (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
  (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
  (let ((gensyms (make-gensym-list (length names) "ONCE-ONLY")))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string n)))) 
                   gensyms names)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ;; bind in user-macro          
          ,(let ,(mapcar #'list names gensyms)
             ,@forms)))))

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (starts-with 'declare current)
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))
