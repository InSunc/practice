(defun read-input-data (filepath)
  (with-open-file (input-stream filepath :direction :input)
    (loop :for line := (read-line input-stream nil)
	  :while (not (null line))
	  :collect line)))

(defun convert-to-elf-ranges (file-lines)
  (flet ((pair-from (range-string)
	   (list (parse-integer (subseq range-string 0 (position #\- range-string)))
		 (parse-integer (subseq range-string (1+ (position #\- range-string)))))))
    (loop :for line :in file-lines
	  :for comma-position := (position #\comma line)
	  :collect (list (pair-from (subseq line 0 comma-position))
			 (pair-from (subseq line (1+ comma-position)))))))

;; Part 1
(defun one-fully-contains-the-other (ranges-pair)
  (let ((range1 (car ranges-pair))
	(range2 (cadr ranges-pair)))
  (or (and (>= (car range1) (car range2))
	   (<= (cadr range1) (cadr range2)))
      (and (>= (car range2) (car range1))
	   (<= (cadr range2) (cadr range1))))))

(defun count-fully-contained-range-pairs (ranges-pair-list)
  (count-if #'one-fully-contains-the-other ranges-pair-list))


;; Part 2
(defun ranges-overlap (ranges-pair)
  (let ((range1 (car ranges-pair))
	(range2 (cadr ranges-pair)))
  (or (and (>= (cadr range1) (car range2))
	   (<= (car range1) (cadr range2)))
      (and (>= (cadr range2) (car range1))
	   (<= (car range2) (cadr range1))))))

(defun count-overlapping-range-pairs (ranges-pair-list)
  (count-if #'ranges-overlap ranges-pair-list))
