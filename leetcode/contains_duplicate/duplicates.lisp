(defun empty? (ls)
  (not ls))

(defun ? (val)
  (not (not val)))

(defun duplicates? (nums)
  (or (? (find (car nums) (cdr nums)))
      (and (not (empty? (cdr nums)))
      	   (duplicates? (cdr nums)))))


(assert (not (duplicates? '(1 2 3 4))))
(assert (duplicates? '(1 2 3 4 1)))
(assert (duplicates? '(1 2 3 4 4)))
(assert (duplicates? '(1 2 3 4 3 4)))
