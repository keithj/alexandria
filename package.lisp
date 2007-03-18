(defpackage :alexandria.0.dev
  (:nicknames :alexandria)
  (:use :cl)
  (:export 
   ;; Binding constructs
   #:if-let
   #:if-let*
   #:when-let
   #:when-let*
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
   #:map-product
   #:flatten
   #:sans
   #:set-equal
   #:setp
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
   #:shuffle
   ;; Macros
   #:with-unique-names   
   #:with-gensyms
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
   ;; Errors
   #:required-argument
   ))
