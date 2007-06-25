(defpackage :alexandria.0.dev
  (:nicknames :alexandria)
  (:use :cl)
  (:export 
   ;; Binding constructs
   #:if-let
   #:if-let*
   #:when-let
   #:when-let*
   ;; Definitions
   #:define-constant
   ;; Control flow
   #:switch
   #:eswitch
   #:cswitch
   #:whichever
   #:xor
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
   #:named-lambda
   ;; Lists
   #:alist-plist
   #:appendf
   #:circular-list
   #:circular-list-p
   #:circular-tree-p
   #:ensure-cons
   #:ensure-list
   #:lastcar
   #:make-circular-list
   #:nunionf
   #:plist-alist
   #:proper-list-p
   #:proper-list
   #:mappend
   #:map-product
   #:flatten
   #:sans
   #:remove-from-plist
   #:remove-from-plistf
   #:delete-from-plist
   #:delete-from-plistf
   #:set-equal
   #:setp
   #:unionf
   ;; Numbers
   #:clamp
   #:gaussian-random
   #:iota   
   #:lerp
   #:map-iota
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
   ;; Strings
   #:string-designator
   ;; Types
   #:of-type
   #:type=
   ;; Errors
   #:required-argument
   ))
