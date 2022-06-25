(defun read-file (filename)
  (with-open-file (stream filename)
    (loop :for line = (read-line stream nil)
    	  :while line
    	  :collect (parse-integer line))))

(defun add-numbers-from-file (filename)
  (reduce '+ (read-file filename)))

(defun first-10-digits (filename)
  (subseq (write-to-string (add-numbers-from-file filename))
  	  0
  	  10))

(print (first-10-digits "numbers"))
