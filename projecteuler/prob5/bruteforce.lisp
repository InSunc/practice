(defun smallest-multiple (start end)
  (let ((i 0))
    (loop :while (not (divisible-by-all (incf i) start end))
    	  :finally (return i))))

(defun divisible-by-all (num start end)
  (loop :for i :from start :to end
        :always (= 0 (mod num i))))

(print (smallest-multiple 1 20))
