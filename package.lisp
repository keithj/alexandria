(defpackage :alexandria.0.dev
  (:nicknames :alexandria)
  (:use :cl)
  (:export
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; BLESSED
   ;;
   ;; Binding constructs
   #:if-let
   #:when-let
   #:when-let*
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; REVIEW IN PROGRESS
   ;;
   ;; Control flow
   #:cswitch
   #:eswitch
   #:switch
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; REVIEW PENDING
   ;;
   #:nth-value-or
   #:whichever
   #:xor
   ;; Definitions
   #:define-constant
   ;; Hash tables
   #:alist-hash-table
   #:copy-hash-table
   #:ensure-gethash
   #:hash-table-alist
   #:hash-table-keys
   #:hash-table-plist
   #:hash-table-values
   #:maphash-keys
   #:maphash-values
   #:plist-hash-table
   ;; Functions
   #:compose
   #:conjoin
   #:curry
   #:disjoin
   #:ensure-function
   #:multiple-value-compose
   #:named-lambda
   #:rcurry
   ;; Lists
   #:alist-plist
   #:appendf
   #:nconcf
   #:circular-list
   #:circular-list-p
   #:circular-tree-p
   #:doplist
   #:ensure-car
   #:ensure-cons
   #:ensure-list
   #:flatten
   #:lastcar
   #:make-circular-list
   #:map-product
   #:mappend
   #:nunionf
   #:plist-alist
   #:proper-list
   #:proper-list-p
   #:remove-from-plist
   #:remove-from-plistf
   #:delete-from-plist
   #:delete-from-plistf
   #:set-equal
   #:setp
   #:unionf
   ;; Numbers
   #:binomial-coefficient
   #:clamp
   #:count-permutations
   #:factorial
   #:gaussian-random
   #:iota
   #:lerp
   #:map-iota
   #:maxf
   #:mean
   #:median
   #:minf
   #:standard-deviation
   #:subfactorial
   #:variance
   ;; Arrays
   #:array-index
   #:array-length
   #:copy-array
   ;; Sequences
   #:copy-sequence
   #:deletef
   #:emptyp
   #:ends-with
   #:ends-with-subseq
   #:first-elt
   #:last-elt
   #:map-combinations
   #:map-derangements
   #:map-permutations
   #:proper-sequence
   #:random-elt
   #:removef
   #:rotate
   #:sequence-of-length-p
   #:length=
   #:shuffle
   #:starts-with
   #:starts-with-subseq
   ;; Macros
   #:once-only
   #:parse-body
   #:parse-ordinary-lambda-list
   #:with-gensyms
   #:with-unique-names
   ;; Symbols
   #:ensure-symbol
   #:format-symbol
   #:make-gensym
   #:make-gensym-list
   #:make-keyword
   ;; Strings
   #:string-designator
   ;; Types
   #:of-type
   #:type=
   #:coercef
   ;; Conditions
   #:required-argument
   #:ignore-some-conditions
   #:simple-style-warning
   #:simple-reader-error
   #:simple-parse-error
   #:simple-program-error
   #:unwind-protect-case
   ;; Features
   #:featurep
   ;; io
   #:with-input-from-file
   #:with-output-to-file
   #:read-file-into-string
   #:write-string-into-file
   #:copy-stream
   #:copy-file
   ;; new additions collected at the end
   #:symbolicate
   ))
