(in-package #:cl-user)

;;; a way to solve part 1 without building any trees at all
;; (defun eliminate (input)
;;   (with-open-file (s input :direction :input)
;;     (loop with rows = (loop for line = (read-line s nil)
;; 			 while line for parsed = (remove #\> (remove #\- (remove #\, line)))
;; 			 for row = (read-from-string (format nil "(~A)" parsed)) collecting row)
;;        for row in rows for node = (car row)
;;        while (find-if #'(lambda (entry) (member node (cddr entry))) rows)
;;        finally (return node))))

(defun build-tree (input)
  (with-open-file (s input :direction :input)
    (let ((rows (sort (loop for line = (read-line s nil)
			 while line for parsed = (remove #\> (remove #\- (remove #\, line)))
			 for row = (read-from-string (format nil "(~A)" parsed)) collecting row)
		      #'<
		      :key #'length)))
      (loop for branch = (pop rows)
	 with matched = nil
	 while rows do
	   (nsubst branch (car branch) rows :test #'(lambda (x &optional y) (when (eql x y) (setf matched t))))
	   (if matched (setf matched nil) (nconc rows (list branch)))
	 finally (return branch)))))

(defun fix-up (tree)
  (let ((weights (loop for child in (cddr tree) collecting (fix-up child))))
    (if weights
	(if (apply #'= weights)
	    (reduce #'+ (append (second tree) weights))
	    (let* ((counts (mapcar #'(lambda (x) (count x weights)) weights))
		   (offender-pos (position-if #'(lambda (x) (= x (apply #'min counts))) counts))
		   (offender-weight (elt weights offender-pos))
		   (correct-weight (elt weights (position-if #'(lambda (x) (= x (apply #'max counts))) counts))))
	      (print (- (car (second (elt tree (+ 2 offender-pos)))) (- offender-weight correct-weight)))
	      (break)))
	(car (second tree))))))
