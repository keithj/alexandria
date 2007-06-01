(in-package :alexandria)

(defmacro switch ((object &key (test 'eql) (key 'identity) (default nil))
                  &body clauses)
  "Evaluates first matching clause, returning its values, or evaluates and
returns the values of DEFAULT if no keys match."
  (with-gensyms (value)
    `(let ((,value (,key ,object)))
       (cond ,@(mapcar (lambda (clause)
                         (destructuring-bind (key-form &body forms) clause
                           `((,test ,value ,key-form)
                             ,@forms)))
                       clauses)
             (t ,default)))))

(defmacro eswitch ((object &key (test 'eql) (key 'identity)) &body clauses)
  "Like SWITCH, but signals an error if no key matches."
  (with-gensyms (value)
    `(let ((,value (,key ,object)))
       (cond ,@(mapcar (lambda (clause)
                         (destructuring-bind (key-form &body forms) clause
                           `((,test ,value ,key-form)
                             ,@forms)))
                       clauses)
             (t
              (error "No keys match in ESWITCH. Testing against ~S with ~S."
                     ,value ',test))))))

(defmacro eswitch ((object &key (test 'eql) (key 'identity)) &body clauses)
  "Like SWITCH, but signals a continuable error if no key matches."
  (with-gensyms (value)
    `(let ((,value (,key ,object)))
       (cond ,@(mapcar (lambda (clause)
                         (destructuring-bind (key-form &body forms) clause
                           `((,test ,value ,key-form)
                             ,@forms)))
                       clauses)
             (t
              (cerror "Return NIL from CSWITCH."
               "No keys match in CSWITCH. Testing against ~S with ~S."
               ,value ',test))))))

(defmacro whichever (&rest possibilities)
  "Evaluates exactly one of POSSIBILITIES, chosen at random."
  `(funcall (the function
              (svref (load-time-value
                      (vector ,@(mapcar (lambda (possibility)
                                          `(lambda () ,possibility))
                                        possibilities))
                      t)
                     (random ,(length possibilities))))))

(defmacro xor (&rest datums)
  "Evaluates its argument one at a time, from left to right. If more then one
argument evaluates to a true value no further DATUMS are evaluated, and NIL is
returned as both primary and secondary value. If exactly one argument
evaluates to true, its value is returned as the primary value after all the
arguments have been evaluated, and T is returned as the secondary value. If no
arguments evaluate to true NIL is retuned as primary, and T as secondary
value."
  (with-gensyms (xor tmp true)
    `(let (,tmp ,true)
       (block ,xor
         ,@(mapcar (lambda (datum)
                     `(if (setf ,tmp ,datum)
                          (if ,true
                              (return-from ,xor (values nil nil))
                              (setf ,true ,tmp))))
                   datums)
         (return-from ,xor (values ,true t))))))