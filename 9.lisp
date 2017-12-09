(in-package #:cl-user)
;;{!{}

(defun solve (input)
  (with-open-file (s input :direction :input)
    (loop for char across (read-line s)
       with garbage = nil and ignore-next = nil
       with count = 0 and score = 0 and trash = 0
       if ignore-next do
	 (setf ignore-next nil)
       else do (cond ((and (not garbage) (eql char #\{)) (incf score))
		     ((and (not garbage) (eql char #\})) (incf count score) (decf score))
		     ((eql char #\<) (if garbage (incf trash) (setf garbage t)))
		     ((eql char #\>) (setf garbage nil))
		     ((eql char #\!) (setf ignore-next t))
		     (t (when garbage (incf trash))))
	 finally (return (cons count trash)))))

