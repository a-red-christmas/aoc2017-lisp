(in-package #:cl-user)

(defun encode (input)
  (with-open-file (s input :direction :input)
    (loop with cypher = (read-from-string (format nil "(~A)" (nsubstitute #\Space #\, (read-line s))))
       with wheel = (loop for i from 0 to 255 collecting i)
       with cursor = (setf (cdr (last wheel)) wheel)
       for skip from 0 for len in cypher do
	 (loop with reverse = (nreverse (loop repeat len for e in cursor collecting e))
	    for i from 0 below len do (setf (nth i cursor) (pop reverse)))
	 (setf cursor (nthcdr (+ len skip) cursor))
       finally (return (* (first wheel) (second wheel))))))

(defun encode2 (input)
  (loop repeat 64
     with cypher = (append (loop for c across input collecting (char-code c)) '(17 31 73 47 23))
     and wheel = (loop for i from 0 to 255 collecting i)
     with cursor = (setf (cdr (last wheel)) wheel) and skip = 0 do
       (loop for len in cypher do
	    (loop with reverse = (nreverse (loop repeat len for e in cursor collecting e))
	       for i from 0 below len do (setf (nth i cursor) (pop reverse)))
	    (setf cursor (nthcdr (+ len skip) cursor))
	    (incf skip))
     finally (return (format nil "~{~2,'0x~}"
			     (loop repeat 16 collecting
				  (reduce #'logxor (loop repeat 16 collecting (pop wheel))))))))
