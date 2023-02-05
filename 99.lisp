;;
;; Source:
;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;;


;; P01 (*) Find the last box of a list.
;;     Example:
;;     * (my-last '(a b c d))
;;     (D)
(defun last-box (list)
  (cond
    ((cdr list) (last-box (cdr list)))
    (t (car list))))

(let ((test-list '(a b c d)))
  (trace last-box)
  (format t "~a" (last-box test-list)))



;; P02 (*) Find the last but one box of a list.
;;     Example:
;;     * (my-but-last '(a b c d))
;;     (C D)
(defun last-but-one-box (list)
  (cond
    ((cddr list) (last-but-one-box (cdr list)))
    (t list)))

(let ((test-list '(a b c d)))
  (trace last-but-one-box)
  (format t "~a" (last-but-one-box test-list)))



;; P03 (*) Find the K'th element of a list.
;;    The first element in the list is number 1.
;;    Example:
;;    * (element-at '(a b c d e) 3)
;;    C
(defun kth (list index)
  (cond
    ((zerop (1- index)) (car list))
    (t (kth (cdr list) (1- index)))))

(let ((tst '(a b c d e))
      (index 3))
  (format t "~a[~a] => ~a~%" tst index (kth tst index)))



;; P04 (*) Find the number of elements of a list.
(defun size (list)
  (labels ((size (list cnt)
	     (cond
	       ((cdr list) (size (cdr list) (1+ cnt)))
	       (t (1+ cnt)))))
    (size list 0)))

(let ((tst '(0 1 2 3 4 5 6 7)))
  (format t "~a => ~a~%" tst (size tst)))



;; P05 (*) Reverse a list.
(defun reverse-of (list)
  (labels ((reverse-of (list reversed-list)
	     (or
	      (and (cdr list) (reverse-of (cdr list)
					    (cons (car list) reversed-list)))
	      (cons (car list) reversed-list))))
    (reverse-of list nil)))

(let ((tst '(1 2 3 4 5)))
  (format t "~a => ~a~%" tst (reverse-of tst)))
  


;; P06 (*) Find out whether a list is a palindrome.
;;     A palindrome can be read forward or backward; e.g. (x a m a x).
(defun palindromep (list)
  (equalp list (reverse-of list)))

(let ((hannah '(h a n n a h))
      (john '(j o h n)))
  (format t "~a: ~a~%" 'hannah (palindromep hannah))
  (format t "~a: ~a~%" 'john (palindromep john)))
  


;; P07 (**) Flatten a nested list structure.
;;     Transform a list, possibly holding lists as elements into a
;;     `flat' list by replacing each list with its elements
;;     (recursively).
;; 
;;     Example:
;;     * (my-flatten '(a (b (c d) e)))
;;     (A B C D E)
;; 
;;     Hint: Use the predefined functions list and append.
(defun flatten (nested-list)
  (let ((head (car nested-list))
	(tail (cdr nested-list)))
    (cond
      ((null nested-list) nil)
      ((listp head) (append (flatten head)
			    (flatten tail)))
      ((not (null tail)) (append (list head)
				 (flatten tail)))
      (t nested-list))))

(let ((tst '((1 2 3) (4 (5 6) 7) 8 (9) ((0)))))
  (format t "~a~%" (flatten tst))
  (format t "~a~%" (flatten nil)))



;; P08 (**) Eliminate consecutive duplicates of list elements.
;;     If a list contains repeated elements they should be replaced
;;     with a single copy of the element. The order of the elements
;;     should not be changed.
;; 
;;     Example:
;;     * (compress '(a a a a b c c a a d e e e e))
;;     (A B C A D E)
(defun compress (list)
  (let ((head (car list))
	(tail (cdr list)))
    (cond
      ((< (size list) 2) list)
      ((not (equalp head (car tail))) (append (list head)
					      (compress tail)))
      (t (compress tail)))))

(let ((t1 '(a a a a b c c a a d e e e e))
      (t2 '(a a))
      (t3 '(a))
      (t4 '()))
  (format t "~a => ~a~%" t1 (compress t1))
  (format t "~a => ~a~%" t2 (compress t2))
  (format t "~a => ~a~%" t3 (compress t3))
  (format t "~a => ~a~%" t4 (compress t4)))



;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;;     If a list contains repeated elements they should be placed in
;;     separate sublists.
;; 
;;     Example:
;;     * (pack '(a a a a b c c a a d e e e e))
;;     ((A A A A) (B) (C C) (A A) (D) (E E E E))
(defun pack (list)
  (let* ((head (car list))
	(tail (cdr list))
	(end-index (position-if-not (lambda (x) (equal x head)) list)))
    (cond
      ((null tail) (list list))
      ((null end-index) (list list))
      (t (append (list (subseq list 0 end-index))
		 (pack (subseq list end-index)))))))

(let ((t1 '(a a a a b c c a a d e e e e))
      (t2 '(a a a))
      (t3 '()))
  (format t "~a => ~a~%" t1 (pack t1))
  (format t "~a => ~a~%" t2 (pack t2))
  (format t "~a => ~a~%" t3 (pack t3)))



;; P10 (*) Run-length encoding of a list.
;;     Use the result of problem P09 to implement the so-called
;;     run-length encoding data compression method. Consecutive
;;     duplicates of elements are encoded as lists (N E) where N is
;;     the number of duplicates of the element E.
;; 
;;     Example:
;;     * (encode '(a a a a b c c a a d e e e e))
;;     ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
(defun encode (list)
  (let* ((head (car list))
	 (tail (cdr list))
	 (end-index (position-if (lambda (x) (not (equal x head))) list)))
    (cond
      ((null tail) nil)
      ((null end-index) `((,(count head list) ,head)))
      (t (append `((,end-index ,head))
		 (encode (subseq list end-index)))))))
  
(let ((t1 '(a a a a b c c a a d e e e e))
      (t2 '(a a a))
      (t3 '()))
  (format t "~a => ~a~%" t1 (encode t1))
  (format t "~a => ~a~%" t2 (encode t2))
  (format t "~a => ~a~%" t3 (encode t3)))


;; P11 (*) Modified run-length encoding.
;;     Modify the result of problem P10 in such a way that if an
;;     element has no duplicates it is simply copied into the result
;;     list. Only elements with duplicates are transferred as (N E)
;;     lists.
;; 
;;     Example:
;;     * (encode-modified '(a a a a b c c a a d e e e e))
;;     ((4 A) B (2 C) (2 A) D (4 E))
(defun encode-modified (list)
  (let* ((head (car list))
	 (tail (cdr list))
	 (end-index (position-if (lambda (x) (not (equal x head))) list)))
    (cond
      ((null tail) nil)
      ((null end-index) `((,(count head list) ,head)))
      ((equalp end-index 1) (append (list head) (encode-modified (subseq list end-index))))
      (t (append `((,end-index ,head))
		 (encode-modified (subseq list end-index)))))))

(let ((t1 '(a a a a b c c a a d e e e e))
      (t2 '(a a a))
      (t3 '()))
  (format t "~a => ~a~%" t1 (encode-modified t1))
  (format t "~a => ~a~%" t2 (encode-modified t2))
  (format t "~a => ~a~%" t3 (encode-modified t3)))


     
;; P12 (**) Decode a run-length encoded list.
;;     Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
(defun decode (encoded-list)
  (flet ((decode-pair (pair)
	   (let ((number-of-elements (car pair))
		 (element (cadr pair)))
	     (make-list number-of-elements :initial-element element))))
    (mapcan #'decode-pair encoded-list)))

(let ((tst '((2 a) (1 b) (3 c))))
  (format t "~a~%" (decode tst)))





;; P13 (**) Run-length encoding of a list (direct solution).
;;   Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
;;
;;   Example:
;;   * (encode-direct '(a a a a b c c a a d e e e e))
;;   ((4 A) B (2 C) (2 A) D (4 E))


;; P14 (*) Duplicate the elements of a list.
;;     Example:
;;     * (dupli '(a b c c d))
;;     (A A B B C C C C D D)
(defun dupli (list)
  (cond
    ((null list) nil)
    (t (cons (car list)
	     (cons (car list)
		   (dupli (cdr list)))))))

(let ((tst '(a a b c)))
  (format t "~a => ~a~%" tst (dupli tst)))


;; P15 (**) Replicate the elements of a list a given number of times.
;;     Example:
;;     * (repli '(a b c) 3)
;;     (A A A B B B C C C)
(defun repli (list replication-count)
  (cond
    ((null list) nil)
    (t (append (make-list replication-count :initial-element (car list))
	       (repli (cdr list) replication-count)))))

(let ((tst '(a a b c)))
  (format t "~a => ~a~%" tst (repli tst 3)))



;; P16 (**) Drop every N'th element from a list.
;;     Example:
;;     * (drop '(a b c d e f g h i k) 3)
;;     (A B D E G H K)
(defun drop (list each-nth &optional (counter 1))
  (cond
    ((null list) nil)
    ((>= counter each-nth) (drop (cdr list) each-nth 1))
    (t (cons (car list) (drop (cdr list) each-nth (1+ counter))))))

(let ((tst '(1 2 3 4 5 6 7 8 9)))
  (format t "~a => ~a~%" tst (drop tst 2))
  (format t "~a => ~a~%" tst (drop tst 1)))



;; P17 (*) Split a list into two parts; the length of the first part is given.
;;     Do not use any predefined functions.
;; 
;;     Example:
;;     * (split '(a b c d e f g h i k) 3)
;;     ( (A B C) (D E F G H I K))
(defun split (list split-index)
  (labels ((split-local (list &optional (counter 0) (left-split t))
	     (cond
	       ((>= counter split-index) (cond
					      (left-split (list (car list)))
					      ((not left-split) (list (cdr list)))))
	       (t (cond
		    (left-split (cons (car list) (split-local (cdr list)
							      (1+ counter)
							      left-split)))
		    ((not left-split) (split-local (cdr list)
						   (1+ counter)
						   left-split)))))))
    (cons (split-local list 0 t)
	  (split-local list 0 nil))))

(let ((tst '(0 1 2 3 4 5 6 7 8 9)))
  (format t "~a => ~a~%" tst (split tst 7))
  (format t "~a => ~a~%" tst (split tst 3))
  (format t "~a => ~a~%" tst (split tst 2)))



;; P18 (**) Extract a slice from a list.
;;     Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
;; 
;;     Example:
;;     * (slice '(a b c d e f g h i k) 3 7)
;;     (C D E F G)
(defun slice (list start-index stop-index)
  (labels ((slice-local (list counter)
	     (cond
	       ((null list) list)
	       ((< counter start-index) (slice-local (cdr list) (1+ counter)))
	       ((= counter stop-index) (list (car list)))
	       (t (cons (car list) (slice-local (cdr list) (1+ counter)))))))
    (slice-local list 1)))

(let ((tst '(a b c d e f g h i k)))
  (format t "~a[3:7] => ~a~%" tst (slice tst 3 7)))


;; P19 (**) Rotate a list N places to the left.
;;     Examples:
;;     * (rotate '(a b c d e f g h) 3)
;;     (D E F G H A B C)
;; 
;;     * (rotate '(a b c d e f g h) -2)
;;     (G H A B C D E F)
;; 
;;     Hint: Use the predefined functions length and append, as well as the result of problem P17.
(defun rotate (list places-nr)
  (let* ((split-index (mod (1- places-nr) (length list)))
	 (sliced-list (split list split-index))
	 (left-slice  (car sliced-list))
	 (right-slice (cadr sliced-list)))
    (append right-slice left-slice)))

(let ((tst '(a b c d e f h))
      (rl0 0)
      (rl1 1)
      (rr1 -1)
      (rl2 2)
      (rr2 -2)
      (rl7 7)
      (rr7 -7)
      (rl10 10)
      (rr10 -10))
  (format t "~a (~a) => ~a~%" tst rl0 (rotate tst rl0))
  (format t "~a (~a) => ~a~%" tst rl1 (rotate tst rl1))
  (format t "~a (~a) => ~a~%" tst rr1 (rotate tst rr1))
  (format t "~a (~a) => ~a~%" tst rl1 (rotate tst rl1))
  (format t "~a (~a) => ~a~%" tst rr1 (rotate tst rr1))
  (format t "~a (~a) => ~a~%" tst rl2 (rotate tst rl2))
  (format t "~a (~a) => ~a~%" tst rr2 (rotate tst rr2))
  (format t "~a (~a) => ~a~%" tst rl7 (rotate tst rl7))
  (format t "~a (~a) => ~a~%" tst rr7 (rotate tst rr7))
  (format t "~a (~a) => ~a~%" tst rl10 (rotate tst rl10))
  (format t "~a (~a) => ~a~%" tst rr10 (rotate tst rr10)))


;; P20 (*) Remove the K'th element from a list.
;;     Example:
;;     * (remove-at '(a b c d) 2)
;;     (A C D)
(defun remove-at (list position)
  (labels ((iterate (list counter)
	     (cond
	       ((>= counter position) (cdr list))
	       (t (cons (car list) (iterate (cdr list) (1+ counter)))))))
    (if (or (<= position 0) (> position (length list)))
	list
	(iterate list 1))))

(let ((tst '(a b c d e f g h)))
  (format t "~a => ~a~%" tst (remove-at tst 0))
  (format t "~a => ~a~%" tst (remove-at tst 100))
  (format t "~a => ~a~%" tst (remove-at tst 5)))



;; P21 (*) Insert an element at a given position into a list.
;;     Example:
;;     * (insert-at 'alfa '(a b c d) 2)
;;     (A ALFA B C D)

(defun insert-at (element list position)
  (labels ((iterate (list counter)
	     (cond
	       ((>= counter position) (cons element list))
	       (t (cons (car list) (iterate (cdr list) (1+ counter)))))))
    (if (or (<= position 0) (> position (length list)))
	list
	(iterate list 1))))

(let ((tst '(a b c d e f g h)))
  (format t "~a => ~a~%" tst (insert-at 'BETA tst 0))
  (format t "~a => ~a~%" tst (insert-at 'BETA tst 100))
  (format t "~a => ~a~%" tst (insert-at 'BETA tst 5)))




;; P22 (*) Create a list containing all integers within a given range.
;;     If first argument is smaller than second, produce a list in decreasing order.
;;     Example:
;;     * (range 4 9)
;;     (4 5 6 7 8 9)

(defun range (lower-limit higher-limit)
  (labels ((generate (inc/dec-func start stop &optional (number-list '()) (number start))
	     (cond
	       ((equal stop number) (cons number number-list))
	       (t (cons number (generate inc/dec-func
					 start
					 stop
					 number-list
					 (funcall inc/dec-func number)))))))
    (if (> lower-limit higher-limit)
	(generate #'1- higher-limit lower-limit)
	(generate #'1+ lower-limit higher-limit))))



;; P23 (**) Extract a given number of randomly selected elements from a list.
;;     The selected items shall be returned in a list.
;;     Example:
;;     * (rnd-select '(a b c d e f g h) 3)
;;     (E D A)
;; 
;;     Hint: Use the built-in random number generator and the result of problem P20.

(defun rnd-select (list elements-to-select &optional (resulting-list '()))
  (if (zerop elements-to-select)
      resulting-list
      (let* ((random-position (1+ (random (length list))))
	     (random-element (kth list random-position))
	     (list-without-random-element (remove-at list random-position)))
	(rnd-select list-without-random-element
		    (1- elements-to-select)
		    (cons random-element resulting-list)))))
			      
     

;; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;;     The selected numbers shall be returned in a list.
;;     Example:
;;     * (lotto-select 6 49)
;;     (23 1 17 33 21 37)
;; 
;;     Hint: Combine the solutions of problems P22 and P23.

(defun lotto-select (numbers-to-return range-end)
    (rnd-select (range 1 range-end) numbers-to-return))




;; P25 (*) Generate a random permutation of the elements of a list.
;;     Example:
;;     * (rnd-permu '(a b c d e f))
;;     (B A D C E F)
;; 
;;     Hint: Use the solution of problem P23.

(defun rnd-permu (list)
  (rnd-select list (length list)))



;; P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
;;     In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
;; 
;;     Example:
;;     * (combination 3 '(a b c d e f))
;;     ((A B C) (A B D) (A B E) ... ) 

(defun combine (combination-length list &optional result)
  (let ((combination-from-list-and-result (equalp combination-length
						  (+ (length list) (length result))))
	(combination-constructed (equalp (length result) combination-length)))
    (cond
      ((null list) (list result))
      (combination-constructed (list result))
      (combination-from-list-and-result (list (append result list)))
      (t (append
	  (combine combination-length
		   (cdr list)
		   (append result (list (car list))))
	  (combine combination-length
		   (cdr list)
		   result))))))

(defun combination (combination-length list &optional (result '()))
  (cond
    ((null list) nil)
    ((< (length list) combination-length) nil)
    ((zerop combination-length) nil)
    (t (append
	(combine combination-length
		 (cdr list)
		 (list (car list)))
	(combination combination-length
		     (cdr list))))))

