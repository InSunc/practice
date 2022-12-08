;;
;; Input data:
;;
;; calories1   ---+
;; calories2      | elf1 calories
;; calories3      |
;; calories4   ---+
;;
;; calories1   ---+
;; calories2      |
;; calories3      | elf2 calories
;; calories4      |
;; calories5   ---+
;;
;; [...]
;;
(defun read-input (filepath)
  (with-open-file (input-stream filepath :direction :input)
    (loop :for line := (read-line input-stream nil)
	  :while line
	  :collect line)))

(defun convert-input-data (file-lines)
  (labels ((split (seq &optional (delimiter "") (test #'string-equal))
	     (let ((delimiter-position (position delimiter seq :test test)))
	       (cond
		 ((null delimiter-position) (list seq))
		 (t (cons (subseq seq 0 delimiter-position)
			  (split (subseq seq (1+ delimiter-position)))))))))
    (loop :for values-group :in (split file-lines)
	  :collect (mapcar #'parse-integer values-group))))


;; Part 1
(defun max-calories (elves-calories-list)
  (loop for elf-calories in elves-calories-list
	:maximize (reduce #'+ elf-calories)))

(max-calories (convert-input-data (read-input "1.input")))


;; Part 2
(defun top-three-elves (elves-calories-list)
  (let ((elves-total-calories-list (mapcar (lambda (x) (reduce #'+ x)) elves-calories-list)))
    (subseq (sort elves-total-calories-list #'>) 0 3)))

(reduce #'+ (top-three-elves (convert-input-data (read-input "1.input"))))
