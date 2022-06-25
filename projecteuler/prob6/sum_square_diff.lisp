(defun sqr (x)
  (* x x))

(defun sum-of-squares (start end)
  (reduce '+
	  (loop :for nr :from start :to end
		collect (sqr nr))))

(defun square-of-sum (start end)
  (sqr
    (reduce '+
	    (loop :for nr :from start :to end
		  collect nr))))

(defun sum-sqr-diff (start end)
  (- (square-of-sum  start end)
     (sum-of-squares start end)))

(print (sum-sqr-diff 1 100))
