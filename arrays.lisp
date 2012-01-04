(in-package :alexandria)

(defun copy-array (array &key (element-type (array-element-type array))
                              (fill-pointer (and (array-has-fill-pointer-p array)
                                                 (fill-pointer array)))
                              (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
 (let* ((dimensions (array-dimensions array))
        (new-array (make-array dimensions
                               :element-type element-type
                               :adjustable adjustable
                               :fill-pointer fill-pointer)))
   (dotimes (i (array-total-size array))
     (setf (row-major-aref new-array i)
           (row-major-aref array i)))
   new-array))

(declaim (inline displace-array))
(defun displace-array (array &key (offset 0)
                                  (dimensions (- (array-total-size array)
                                               offset)))
  "Return an array displaced to ARRAY with the given OFFSET and DIMENSIONS.
Default arguments displace to a vector."
  (make-array dimensions
              :displaced-to array
              :displaced-index-offset offset
              :element-type (array-element-type array)))
