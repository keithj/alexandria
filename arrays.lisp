(in-package :alexandria)

(deftype array-index (&optional (length array-dimension-limit))
  "Type designator for an index into array of LENGTH: an integer between
0 (inclusive) and LENGTH (exclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT."
  `(integer 0 (,length)))

(deftype array-length (&optional (length array-dimension-limit))
  "Type designator for a dimension of an array of LENGTH: an integer between
0 (inclusive) and LENGTH (inclusive). LENGTH defaults to
ARRAY-DIMENSION-LIMIT."
  `(integer 0 ,length))

(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer
and adjustability (if any) as the original, unless overridden by
the keyword arguments."
  (let ((dims (array-dimensions array)))
    ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
    ;; displaced array to a non-displaced one to make a copy.
    (adjust-array
     (make-array dims
                 :element-type element-type :fill-pointer fill-pointer
                 :adjustable adjustable :displaced-to array)
     dims)))

