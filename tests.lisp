(in-package :cl-user)

(require :sb-rt)

(require :alexandria)

(defpackage :alexandria-test
  (:use :cl :alexandria :sb-rt))

(in-package :alexandria-test)

;;;; Hash tables

(deftest copy-hash-table.1
    (let ((orig (make-hash-table :test 'eq :size 123))
          (foo "foo"))
      (setf (gethash orig orig) t
            (gethash foo orig) t)
      (let ((eq-copy (copy-hash-table orig))
            (eql-copy (copy-hash-table orig :test 'eql))
            (equal-copy (copy-hash-table orig :test 'equal))
            (equalp-copy (copy-hash-table orig :test 'equalp)))
        (list (hash-table-size eq-copy)
              (hash-table-count eql-copy)
              (gethash orig eq-copy)
              (gethash (copy-seq foo) eql-copy)
              (gethash foo eql-copy)
              (gethash (copy-seq foo) equal-copy)
              (gethash "FOO" equal-copy)
              (gethash "FOO" equalp-copy))))
  (123 2 t nil t t nil t))

(deftest maphash-keys.1
    (let ((keys nil)
          (table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash i table) t))
      (maphash-keys (lambda (k) (push k keys)) table)
      (set-equal keys '(0 1 2 3 4 5 6 7 8 9)))
  t)

(deftest maphash-values.1
    (let ((vals nil)
          (table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash i table) (- i)))
      (maphash-values (lambda (v) (push v vals)) table)
      (set-equal vals '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)))  
  t)

(deftest hash-table-keys.1
    (let ((table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash i table) t))
      (set-equal (hash-table-keys table) '(0 1 2 3 4 5 6 7 8 9)))
  t)

(deftest hash-table-values.1
    (let ((table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash (gensym) table) i))
      (set-equal (hash-table-values table) '(0 1 2 3 4 5 6 7 8 9)))
  t)

(deftest hash-table-alist.1
    (let ((table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash i table) (- i)))
      (let ((alist (hash-table-alist table)))
        (list (length alist)
              (assoc 0 alist)
              (assoc 3 alist)
              (assoc 9 alist)
              (assoc nil alist))))
  (10 (0 . 0) (3 . -3) (9 . -9) nil))

(deftest hash-table-plist.1
    (let ((table (make-hash-table)))
      (dotimes (i 10)
        (setf (gethash i table) (- i)))
      (let ((plist (hash-table-plist table)))
        (list (length plist)
              (getf plist 0)
              (getf plist 2)
              (getf plist 7)
              (getf plist nil))))
  (20 0 -2 -7 nil))

(deftest alist-hash-table.1
    (let* ((alist '((0 a) (1 b) (2 c)))
           (table (alist-hash-table alist)))
      (list (hash-table-count table)
            (gethash 0 table)
            (gethash 1 table)
            (gethash 2 table)
            (hash-table-test table)))
  (3 (a) (b) (c) eql))

(deftest plist-hash-table.1
    (let* ((plist '(:a 1 :b 2 :c 3))
           (table (plist-hash-table plist :test 'eq)))
      (list (hash-table-count table)
            (gethash :a table)
            (gethash :b table)
            (gethash :c table)
            (gethash 2 table)
            (gethash nil table)
            (hash-table-test table)))
  (3 1 2 3 nil nil eq))

;;;; Functions

(deftest disjoin.1
    (let ((disjunction (disjoin (lambda (x)
                                  (and (consp x) :cons))
                                (lambda (x)
                                  (and (stringp x) :string)))))
      (list (funcall disjunction 'zot)
            (funcall disjunction '(foo bar))
            (funcall disjunction "test")))
  (nil :cons :string))

(deftest conjoin.1
    (let ((conjunction (conjoin #'consp 
                                (lambda (x)
                                  (stringp (car x)))
                                (lambda (x)
                                  (char (car x) 0)))))
      (list (funcall conjunction 'zot)
            (funcall conjunction '(foo))
            (funcall conjunction '("foo"))))
  (nil nil #\f))

(deftest compose.1
    (let ((composite (compose '1+ 
                              (lambda (x)
                                (* x 2))
                              #'read-from-string)))
      (funcall composite "1"))
  3)

(deftest compose.2
    (let ((composite 
           (locally (declare (notinline compose))
             (compose '1+ 
                      (lambda (x)
                        (* x 2))
                      #'read-from-string))))
      (funcall composite "2"))
  5)

(deftest compose.3
    (let ((compose-form (funcall (compiler-macro-function 'compose)
                                 '(compose '1+ 
                                   (lambda (x)
                                     (* x 2))
                                   #'read-from-string)
                                 nil)))
      (let ((fun (funcall (compile nil `(lambda () ,compose-form)))))
        (funcall fun "3")))
  7)

(deftest multiple-value-compose.1
    (let ((composite (multiple-value-compose
                      #'truncate
                      (lambda (x y)
                        (values y x))
                      (lambda (x)
                        (with-input-from-string (s x)
                          (values (read s) (read s)))))))
      (multiple-value-list (funcall composite "2 7")))
  (3 1))

(deftest multiple-value-compose.2
    (let ((composite (locally (declare (notinline multiple-value-compose))
                       (multiple-value-compose
                        #'truncate
                        (lambda (x y)
                          (values y x))
                       (lambda (x)
                         (with-input-from-string (s x)
                           (values (read s) (read s))))))))
      (multiple-value-list (funcall composite "2 11")))
  (5 1))

(deftest multiple-value-compose.3
    (let ((compose-form (funcall (compiler-macro-function 'multiple-value-compose)
                                 '(multiple-value-compose
                                   #'truncate
                                   (lambda (x y)
                                     (values y x))
                                   (lambda (x)
                                     (with-input-from-string (s x)
                                       (values (read s) (read s)))))
                                 nil)))
      (let ((fun (funcall (compile nil `(lambda () ,compose-form)))))
        (multiple-value-list (funcall fun "2 9"))))
  (4 1))

(deftest curry.1 
    (let ((curried (curry '+ 3)))
      (funcall curried 1 5))
  9)

(deftest curry.2
    (let ((curried (locally (declare (notinline curry))
                     (curry '* 2 3))))
      (funcall curried 7))
  42)

(deftest curry.3
    (let ((curried-form (funcall (compiler-macro-function 'curry)
                                 '(curry '/ 8)
                                 nil)))
      (let ((fun (funcall (compile nil `(lambda () ,curried-form)))))
        (funcall fun 2)))
  4)

(deftest rcurry.1
    (let ((r (rcurry '/ 2)))
      (funcall r 8))
  4)

;;;; Lists

(deftest appendf.1
    (let* ((list '(1 2 3))
           (orig list))
      (appendf list '(4 5 6) '(7 8))
      (list list (eq list orig)))
  ((1 2 3 4 5 6 7 8) nil))

(deftest circular-list.1
    (let ((circle (circular-list 1 2 3)))
      (list (first circle)
            (second circle)
            (third circle)
            (fourth circle)
            (eq circle (nthcdr 3 circle))))
  (1 2 3 1 t))

(deftest circular-list-p.1
    (let* ((circle (circular-list 1 2 3 4))
           (tree (list circle circle))
           (dotted (cons circle t))
           (proper (list 1 2 3 circle))
           (tailcirc (list* 1 2 3 circle)))
      (list (circular-list-p circle)
            (circular-list-p tree)
            (circular-list-p dotted)
            (circular-list-p proper)
            (circular-list-p tailcirc)))
  (t nil nil nil nil t))

(deftest circular-tree-p.1
    (let* ((circle (circular-list 1 2 3 4))
           (tree1 (list circle circle))
           (tree2 (let* ((level2 (list 1 nil 2))
                         (level1 (list level2)))
                    (setf (second level2) level1)
                    level1))
           (dotted (cons circle t))
           (proper (list 1 2 3 circle))
           (tailcirc (list* 1 2 3 circle))
           (quite-proper (list 1 2 3))
           (quite-dotted (list 1 (cons 2 3))))
      (list (circular-tree-p circle)
            (circular-tree-p tree1)            
            (circular-tree-p tree2)
            (circular-tree-p dotted)
            (circular-tree-p proper)
            (circular-tree-p tailcirc)
            (circular-tree-p quite-proper)
            (circular-tree-p quite-dotted)))
  (t t t t t t nil nil))

(deftest proper-list-p.1
    (let ((l1 (list 1))
          (l2 (list 1 2))
          (l3 (cons 1 2))
          (l4 (list (cons 1 2) 3))
          (l5 (circular-list 1 2)))
      (list (proper-list-p l1)
            (proper-list-p l2)
            (proper-list-p l3)
            (proper-list-p l4)
            (proper-list-p l5)))
  (t t nil t nil))

(deftest proper-list.type.1
    (let ((l1 (list 1))
          (l2 (list 1 2))
          (l3 (cons 1 2))
          (l4 (list (cons 1 2) 3))
          (l5 (circular-list 1 2)))
      (list (typep l1 'proper-list)
            (typep l2 'proper-list)
            (typep l3 'proper-list)
            (typep l4 'proper-list)
            (typep l5 'proper-list)))
  (t t nil t nil))

(deftest lastcar.1
    (let ((l1 (list 1))
          (l2 (list 1 2)))
      (list (lastcar l1)
            (lastcar l2)))
  (1 2))

(deftest lastcar.error.2
    (handler-case
        (progn 
          (lastcar (circular-list 1 2 3))
          nil)
      (error ()
        t))
  t)

(deftest make-circular-list.1
    (let ((l (make-circular-list 3 :initial-element :x)))
      (setf (car l) :y)
      (list (eq l (nthcdr 3 l))
            (first l)
            (second l)
            (third l)
            (fourth l)))
  (t :y :x :x :y))

(deftest circular-list.type.1
    (let* ((l1 (list 1 2 3))
           (l2 (circular-list 1 2 3))
           (l3 (list* 1 2 3 l2)))
      (list (typep l1 'circular-list)
            (typep l2 'circular-list)
            (typep l3 'circular-list)))
  (nil t t))

(deftest ensure-list.1
    (let ((x (list 1))
          (y 2))
      (list (ensure-list x)
            (ensure-list y)))
  ((1) (2)))

(deftest remove-keys.1
    (let* ((orig '(a 1 b 2 c 3 d 4))
           (copy (copy-seq orig)))
      (list (remove-keys '(a c) copy)
            (remove-keys '(b d) copy)
            (remove-keys '(b) copy)
            (remove-keys '(a) copy)
            (remove-keys '(d) copy)
            (remove-keys '(a b c d) copy)
            (remove-keys '(a b c d e) copy)
            (equal copy orig)))
  ((b 2 d 4)
   (a 1 c 3)
   (a 1 c 3 d 4)
   (b 2 c 3 d 4)
   (a 1 b 2 c 3)
   nil
   nil
   t))

(deftest mappend.1
    (mappend (compose 'list '*) '(1 2 3) '(1 2 3))
  (1 4 9))

;;;; Numbers

(deftest clamp.1
    (list (clamp 1.5 1 2)
          (clamp 2.0 1 2)
          (clamp 1.0 1 2)
          (clamp 3 1 2)
          (clamp 0 1 2))
  (1.5 2.0 1.0 2 1))

#+(or)
(deftest gaussian-random.1
    ???
  )

(deftest iota.1
    (iota 3)
  (0 1 2))

(deftest iota.2
    (iota 3 :start 0.0d0)
  (0.0d0 1.0d0 2.0d0))

(deftest iota.3
    (iota 3 :start 2 :step 3.0)
  (2 5.0 8.0))

(deftest lerp.1
    (lerp 0.5 1 2)
  1.5)

(deftest lerp.2
    (lerp 0.1 1 2)
  1.1)

(deftest mean.1
    (mean '(1 2 3))
  2)

(deftest mean.2
    (mean '(1 2 3 4))
  5/2)

(deftest mean.3 
    (mean '(1 2 10))
  13/2)

(deftest median.1
    (median '(100 0 99 1 98 2 97))
  97)

(deftest median.2
    (median '(100 0 99 1 98 2 97 96))
  195/2)

#+(or)
(deftest variance)

#+nil
(deftest standard-deviation)

(deftest maxf.1
    (let ((x 1))
      (maxf x 2)
      x)
  2)

(deftest maxf.2
    (let ((x 1))
      (maxf x 0)
      x)
  1)

(deftest maxf.3
    (let ((x 1)
          (c 0))
      (maxf x (incf c))
      (list x c))
  (1 1))

(deftest maxf.4
    (let ((xv (vector 0 0 0))
          (p 0))
      (maxf (svref xv (incf p)) (incf p))
      (list p xv))
  (2 #(0 2 0)))

(deftest minf.1
    (let ((y 1))
      (minf y 0)
      y)
  0)

(deftest minf.2
    (let ((xv (vector 10 10 10))
          (p 0))
      (minf (svref xv (incf p)) (incf p))
      (list p xv))
  (2 #(10 2 10)))

;;;; Arrays

#+nil
(deftest array-index.type)

#+nil
(deftest copy-array)

;;;; Sequences

(deftest rotate-left.1
    (list (rotate-left (list 1 2 3) 0)
          (rotate-left (list 1 2 3))
          (rotate-left (list 1 2 3) 2)
          (rotate-left (list 1 2 3) 3)
          (rotate-left (list 1 2 3) 4))
  ((1 2 3)
   (3 1 2)
   (2 3 1)
   (1 2 3)
   (3 1 2)))

(deftest rotate-left.2
    (list (rotate-left (vector 1 2 3 4) 0)
          (rotate-left (vector 1 2 3 4))
          (rotate-left (vector 1 2 3 4) 2)
          (rotate-left (vector 1 2 3 4) 3)
          (rotate-left (vector 1 2 3 4) 4)
          (rotate-left (vector 1 2 3 4) 5))
  (#(1 2 3 4)
    #(4 1 2 3)
    #(3 4 1 2)
    #(2 3 4 1)
    #(1 2 3 4)
    #(4 1 2 3)))

(deftest rotate-right.1
    (list (rotate-right (list 1 2 3) 0)
          (rotate-right (list 1 2 3))
          (rotate-right (list 1 2 3) 2)
          (rotate-right (list 1 2 3) 3)
          (rotate-right (list 1 2 3) 4))
  ((1 2 3)
   (2 3 1)
   (3 1 2)
   (1 2 3)
   (2 3 1)))

(deftest rotate-right.2
    (list (rotate-right (vector 1 2 3 4) 0)
          (rotate-right (vector 1 2 3 4))
          (rotate-right (vector 1 2 3 4) 2)
          (rotate-right (vector 1 2 3 4) 3)
          (rotate-right (vector 1 2 3 4) 4)
          (rotate-right (vector 1 2 3 4) 5))
  (#(1 2 3 4)
   #(2 3 4 1)
   #(3 4 1 2)
   #(4 1 2 3)
   #(1 2 3 4)
   #(2 3 4 1)))

(deftest suffle.1
    (let ((s (suffle (iota 100))))
      (list (equal s (iota 100))
            (every (lambda (x)
                     (member x s))
                   (iota 100))
            (every (lambda (x)
                     (typep x '(integer 0 99)))
                   s)))
  (nil t t))

(deftest random-elt.1
    (let ((s1 #(1 2 3 4))
          (s2 '(1 2 3 4)))
      (list (dotimes (i 1000 nil)
              (unless (member (random-elt s1) s2)
                (return nil))
              (when (/= (random-elt s1) (random-elt s1))
                (return t)))
            (dotimes (i 1000 nil)
              (unless (member (random-elt s2) s2)
                (return nil))
              (when (/= (random-elt s2) (random-elt s2))
                (return t)))))
  (t t))

(deftest removef.1
    (let* ((x '(1 2 3))
           (x* x)
           (y #(1 2 3))
           (y* y))
      (removef x 1)
      (removef y 3)
      (list x x* y y*))
  ((2 3)
   (1 2 3)
   #(1 2)
   #(1 2 3)))

(deftest deletef.1
    (let* ((x '(1 2 3))
           (x* x)
           (y #(1 2 3)))
      (deletef x 2)
      (deletef y 1)
      (list x x* y))
  ((1 3)
   (1 3)
   #(2 3)))

(deftest proper-sequence.type.1
    (mapcar (lambda (x)
              (typep x 'proper-sequence))
            (list (list 1 2 3)
                  (vector 1 2 3)
                  #2a((1 2) (3 4))
                  (circular-list 1 2 3 4)))
  (t t nil nil))

(deftest emptyp.1
    (mapcar #'emptyp 
            (list (list 1)
                  (circular-list 1)
                  nil
                  (vector)
                  (vector 1)))
  (nil nil t t nil))

(deftest sequence-of-length-p.1
    (mapcar #'sequence-of-length-p
            (list nil
                  #()
                  (list 1)
                  (vector 1)
                  (list 1 2)
                  (vector 1 2)
                  (list 1 2)
                  (vector 1 2)
                  (list 1 2)
                  (vector 1 2))
            (list 0
                  0
                  1
                  1
                  2
                  2
                  1
                  1
                  4
                  4))
  (t t t t t t nil nil nil nil))

(deftest copy-sequence.1
    (let ((l (list 1 2 3))
          (v (vector #\a #\b #\c)))
      (let ((l.list (copy-sequence 'list l))
            (l.vector (copy-sequence 'vector l))
            (l.spec-v (copy-sequence '(vector fixnum) l))
            (v.vector (copy-sequence 'vector v))
            (v.list (copy-sequence 'list v))
            (v.string (copy-sequence 'string v)))
        (list (member l (list l.list l.vector l.spec-v))
              (member v (list v.vector v.list v.string))
              (equal l.list l)
              (equalp l.vector #(1 2 3))
              (eq 'fixnum (array-element-type l.spec-v))
              (equalp v.vector v)
              (equal v.list '(#\a #\b #\c))
              (equal "abc" v.string))))
  (nil nil t t t t t t))

(deftest first-elt.1
    (mapcar #'first-elt
            (list (list 1 2 3)
                  "abc"
                  (vector :a :b :c)))
  (1 #\a :a))

(deftest first-elt.error.1
    (mapcar (lambda (x)
              (handler-case
                  (first-elt x)
                (type-error ()
                  :type-error)))
            (list nil
                  #()
                  12
                  :zot))
  (:type-error
   :type-error
   :type-error
   :type-error))

(deftest last-elt.1
    (mapcar #'last-elt
            (list (list 1 2 3)
                  (vector :a :b :c)
                  "FOOBAR"
                  #*001
                  #*010))
  (3 :c #\R 1 0))

(deftest last-elt.error.1
    (mapcar (lambda (x)
              (handler-case
                  (last-elt x)
                (type-error ()
                  :type-error)))
            (list nil
                  #()
                  12
                  :zot
                  (circular-list 1 2 3)
                  (list* 1 2 3 (circular-list 4 5))))
  (:type-error 
   :type-error 
   :type-error 
   :type-error 
   :type-error
   :type-error))

(deftest starts-with.1
    (list (starts-with 1 '(1 2 3))
          (starts-with 1 #(1 2 3))
          (starts-with #\x "xyz")
          (starts-with 2 '(1 2 3))
          (starts-with 3 #(1 2 3))
          (starts-with 1 1)
          (starts-with nil nil))
  (t t t nil nil nil nil))

(deftest ends-with.1
    (list (ends-with 3 '(1 2 3))
          (ends-with 3 #(1 2 3))
          (ends-with #\z "xyz")
          (ends-with 2 '(1 2 3))
          (ends-with 1 #(1 2 3))
          (ends-with 1 1)
          (ends-with nil nil))
  (t t t nil nil nil nil))

(deftest ends-with.error.1
    (handler-case
        (ends-with 3 (circular-list 3 3 3 1 3 3))
      (type-error ()
        :type-error))
  :type-error)

(deftest with-unique-names.1
    (let ((*gensym-counter* 0))
      (let ((syms (with-unique-names (foo bar quux)
                    (list foo bar quux))))
        (list (find-if #'symbol-package syms)
              (equal '("FOO0" "BAR1" "QUUX2")
                     (mapcar #'symbol-name syms)))))
  (nil t))

(deftest once-only.1
    (macrolet ((cons1.good (x)
                 (once-only (x)
                   `(cons ,x ,x)))
               (cons1.bad (x)
                 `(cons ,x ,x)))
      (let ((y 0))
        (list (cons1.good (incf y))
              y
              (cons1.bad (incf y))
              y)))
  ((1 . 1) 1 (2 . 3) 3))

(deftest parse-body.1
    (parse-body '("doc" "body") :documentation t)
  ("body") 
  nil 
  "doc")

(deftest parse-body.2
    (parse-body '("body") :documentation t)
  ("body") 
  nil 
  nil)

(deftest parse-body.3
    (parse-body '("doc" "body"))
  ("doc" "body") 
  nil 
  nil)

(deftest parse-body.4
    (parse-body '((declare (foo)) "doc" (declare (bar)) body) :documentation t)
  (body)
  ((declare (foo)) (declare (bar)))
  "doc")

(deftest parse-body.5
    (parse-body '((declare (foo)) "doc" (declare (bar)) body))
  ("doc" (declare (bar)) body)
  ((declare (foo)))
  nil)

;;;; Symbols

(deftest ensure-symbol.1
    (ensure-symbol :cons :cl)
  cons)

(deftest ensure-symbol.2
    (ensure-symbol "CONS")
  cons)

(deftest ensure-symbol.3
    (ensure-symbol 'foo :keyword)
  :foo)

(deftest ensure-symbol.4
    (ensure-symbol #\*)
  *)

(deftest format-symbol.1
    (let ((s (format-symbol nil "X-~D" 13)))
      (list (symbol-package s)
            (symbol-name s)))
  (nil "X-13"))

(deftest format-symbol.2
    (format-symbol :keyword "SYM-~A" :bolic)
  :sym-bolic) 

(deftest format-symbol.3
    (let ((*package* (find-package :cl)))
      (format-symbol t "FIND-~A" 'package))
  find-package)

(deftest make-keyword.1
    (list (make-keyword 'zot)
          (make-keyword "FOO")
          (make-keyword #\Q))
  (:zot :foo :q))

(deftest make-gensym-list.1
    (let ((*gensym-counter* 0))
      (let ((syms (make-gensym-list 3 "FOO")))
        (list (find-if 'symbol-package syms)
              (equal '("FOO0" "FOO1" "FOO2")
                     (mapcar 'symbol-name syms)))))
  (nil t))

;;;; Type-system

(deftest of-type.1
    (let ((f (of-type 'string)))
      (list (funcall f "foo")
            (funcall f 'bar)))
  (t nil))

(deftest type=.1
    (type= 'string 'string)
  t
  t)

(deftest type=.2
    (type= 'list '(or null cons))
  t
  t)

(deftest type=.3
    (type= 'null '(and symbol list))
  t
  t)

(deftest type=.4
    (type= 'string '(satisfies emptyp))
  nil
  nil)

(deftest type=.5
    (type= 'string 'list)
  nil
  t)
