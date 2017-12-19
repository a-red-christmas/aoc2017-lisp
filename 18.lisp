(in-package #:cl-user)

(defun interpret (input)
  (with-open-file (s input :direction :input)
    (let*  ((contents (loop for line = (read-line s nil) for entry = (read-from-string (format nil "(~A)" line))
			 while line collecting entry))
	    (program (make-array (length contents)
				 :initial-contents contents))
	    (registers (make-hash-table)))
      (flet ((fetch (arg)
	       (if (numberp arg)
		   arg
		   (gethash arg registers 0))))
	(loop with pc = 0
	   with sound = 0
	   for command = (aref program pc)
	   while (<= 0 pc (1- (length program))) do
	     (case (first command)
	       (set (setf (gethash (second command) registers) (fetch (third command))))
	       (add (incf (gethash (second command) registers 0) (fetch (third command))))
	       (mul (setf (gethash (second command) registers) (* (fetch (second command)) (fetch (third command)))))
	       (mod (setf (gethash (second command) registers) (mod (fetch (second command)) (fetch (third command)))))
	       (snd (setf sound (fetch (second command))))
	       (rcv (when (plusp (fetch (second command))) (return sound)))
	       (jgz (when (plusp (fetch (second command)))
		      (incf pc (1- (fetch (third command)))))))
	     (incf pc))))))

;; pt2 in progress

(defun make-interpreter (program id queues &optional counter)
  (let ((registers (make-hash-table))
	(pc 0)
	(dest-reg nil))
    (flet ((fetch (arg)
	     (if (numberp arg)
		 arg
		 (gethash arg registers 0))))
      (setf (gethash 'p registers) id)
      #'(lambda (message)
	  (when dest-reg
	    (setf (gethash dest-reg registers) message)
	    (incf pc))
	  (loop for command = (print (aref program pc))
	     while (<= 0 pc (1- (length program))) do
	       (case (first command)
		 (set (setf (gethash (second command) registers) (fetch (third command))))
		 (add (incf (gethash (second command) registers 0) (fetch (third command))))
		 (mul (setf (gethash (second command) registers) (* (fetch (second command)) (fetch (third command)))))
		 (mod (setf (gethash (second command) registers) (mod (fetch (second command)) (fetch (third command)))))
		 (snd (push (fetch (second command)) (aref queues id)) (when counter (funcall counter)))
		 (rcv (setf dest-reg (fetch (second command))) (return))
		 (jgz (when (plusp (fetch (second command)))
			(incf pc (1- (fetch (third command)))))))
	       (incf pc))))))

(defun interpret2 (input)
  (with-open-file (s input :direction :input)
    (let*  ((contents (loop for line = (read-line s nil) for entry = (read-from-string (format nil "(~A)" line))
			 while line collecting entry))
	    (program (make-array (length contents)
				 :initial-contents contents))
	    (blocks (make-array 2 :initial-element nil))
	    (queues (make-array 2 :initial-contents '(()())))
	    (count 0)
	    (processes (make-array 2 :initial-contents (list (make-interpreter program 0 queues)
							     (make-interpreter program 1 queues
									       #'(lambda () (incf count)))))))
      (loop
	 for start = t then (and start (= pid 1)) ;; t for first two calls
	 for pid = 0 then alter
	 for alter = (if (zerop pid) 1 0)
	 for qin = (aref queues alter) do
	   (when (and (not start) (null (aref queues pid)) (null (aref queues alter)))
	     (return count))
	   (handler-case (funcall (aref processes pid) (car (last qin)))
	     (error () (return (+ count (length (aref queues 0))))))
	   (setf (aref blocks pid) nil
		 (aref queues alter) (butlast qin))))))
