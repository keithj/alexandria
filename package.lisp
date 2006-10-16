(defpackage :alexandria.0.dev
  (:nicknames :alexandria)
  (:use :cl)
  (:export 
   ;; Hash tables
   #:copy-hash-table
   #:hash-table-keys
   #:hash-table-values
   #:hash-table-plist
   #:hash-table-alist
   #:alist-hash-table
   #:plist-hash-table
   #:maphash-keys
   #:maphash-values
   ;; Functions
   #:conjoin
   #:disjoin
   #:curry
   #:rcurry
   #:compose
   #:multiple-value-compose
   ;; Lists
   #:appendf
   #:circular-list
   #:circular-list-p
   #:circular-tree-p
   #:ensure-list
   #:lastcar
   #:make-circular-list
   #:proper-list-p
   #:proper-list
   #:mappend
   #:sans
   #:set-equal
   ;; Numbers
   #:clamp
   #:gaussian-random
   #:iota
   #:lerp
   #:maxf
   #:mean   
   #:median
   #:minf
   #:variance
   #:standard-deviation
   ;; Arrays
   #:array-index
   #:copy-array
   ;; Sequences
   #:emptyp
   #:copy-sequence
   #:first-elt
   #:last-elt
   #:starts-with
   #:ends-with
   #:removef
   #:deletef
   #:proper-sequence
   #:random-elt
   #:rotate
   #:sequence-of-length-p
   #:suffle
   ;; Macros
   #:with-unique-names   
   #:once-only
   #:parse-body
   ;; Symbols
   #:ensure-symbol
   #:format-symbol
   #:make-keyword
   #:make-gensym-list
   ;; Types
   #:of-type
   #:type=
   ))
