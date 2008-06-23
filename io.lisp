;; Copyright (c) 2002-2006, Edward Marco Baringer
;; All rights reserved.

(in-package :alexandria)

(defmacro with-input-from-file ((stream-name file-name &rest args &key
					     (direction nil direction-provided-p)
					     &allow-other-keys)
                                &body body)
  "Evaluate BODY with STREAM-NAME bound to an input-stream from file
FILE-NAME. ARGS is passed directly to open."
  (declare (ignore direction))
  (when direction-provided-p
    (error "Can't specifiy :DIRECTION in WITH-INPUT-FROM-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :input ,@args)
     ,@body))

(defmacro with-output-to-file ((stream-name file-name &rest args &key
					     (direction nil direction-provided-p)
					     &allow-other-keys)
			       &body body)
  "Evaluate BODY with STREAM-NAME to an output stream on the file
FILE-NAME. ARGS is sent as is to the call te open."
  (declare (ignore direction))
  (when direction-provided-p
    (error "Can't specifiy :DIRECTION in WITH-OUTPUT-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :output ,@args)
     ,@body))

(defun read-file-into-string (pathname &key (buffer-size 4096) (external-format :default))
  "Return the contents of PATHNAME as a fresh string.

The file specified by PATHNAME will be read one ELEMENT-TYPE
element at a time, the EXTERNAL-FORMAT and ELEMENT-TYPEs must be
compatible.

The EXTERNAL-FORMAT parameter will be passed to
ENCODING-KEYWORD-TO-NATIVE, see ENCODING-KEYWORD-TO-NATIVE to
possible values."
  (with-input-from-file
      (file-stream pathname :external-format external-format)
    (let ((*print-pretty* nil))
      (with-output-to-string (datum)
        (let ((buffer (make-array buffer-size :element-type 'character)))
	  (loop
	     :for bytes-read = (read-sequence buffer file-stream)
	     :do (write-sequence buffer datum :start 0 :end bytes-read)
	     :while (= bytes-read buffer-size)))))))

(defun write-string-into-file (string pathname &key (if-exists :error)
						    (if-does-not-exist :error)
						    (external-format :default))
  "Write STRING to PATHNAME.

The EXTERNAL-FORMAT parameter will be passed to
ENCODING-KEYWORD-TO-NATIVE, see ENCODING-KEYWORD-TO-NATIVE to
possible values."
  (with-output-to-file (file-stream pathname :if-exists if-exists
				    :if-does-not-exist if-does-not-exist
				    :external-format external-format)
    (write-sequence string file-stream)))

(defun copy-file (from to &key (if-to-exists :supersede)
			       (element-type '(unsigned-byte 8)))
  (with-input-from-file (input  from :element-type element-type)
    (with-output-to-file (output to :element-type element-type
				    :if-exists if-to-exists)
      (copy-stream input output))))

(defun copy-stream (input output &optional (element-type (stream-element-type input)))
  "Reads data from FROM and writes it to TO. Both FROM and TO must be streams,
they will be passed to read-sequence/write-sequence and must have compatable
element-types."
  (loop
     :with buffer-size = 4096
     :with buffer = (make-array buffer-size :element-type element-type)
     :for bytes-read = (read-sequence buffer input)
     :while (= bytes-read buffer-size)
     :do (write-sequence buffer output)
     :finally (write-sequence buffer output :end bytes-read)))
