(defun lower-case-priority-list ()
  (loop :for char
	  :from (char-code #\a)
	    :to (char-code #\z)
	:for priority :from 1
	:collect (list (code-char char) priority)))

(defun upper-case-priority-list ()
  (loop :for char
	  :from (char-code #\A)
	    :to (char-code #\Z)
	:for priority :from 27
	:collect (list (code-char char) priority)))

(defconstant +items-priority+ (concatenate 'list
					   (lower-case-priority-list)
					   (upper-case-priority-list)))
(defun item-priority (item)
	(cadr (assoc item +items-priority+)))

(defun read-input-data (filepath)
  (with-open-file (input-stream filepath :direction :input)
    (loop :for line := (read-line input-stream nil)
	  :while (not (null line))
	  :collect line)))

(defun item-set-from (items-string)
	(remove-duplicates (coerce items-string 'list)))


;; Part 1
(defun convert-to-rucksacks (file-lines)
  (loop :for line :in file-lines
	:collect (list (subseq line 0 (/ (length line) 2))
		       (subseq line (/ (length line) 2)))))

(defun compartments-common-items (rucksack)
    (intersection (item-set-from (car rucksack))
		  (item-set-from (cadr rucksack))))

(defun priority-sum (rucksacks)
  (loop :for rucksack :in rucksacks
	:for common-item := (car (compartments-common-items rucksack))
	:if (not (null common-item))
	  :collect (item-priority common-item) :into item-priorities
	:finally (return (reduce #'+ item-priorities))))

(priority-sum (convert-to-rucksacks (read-input-data "3.input")))


;; Part 2
(defun convert-to-elf-groups (file-lines)
  (flet ((partition (list number-of-elements)
	   (loop :for tail :on list :by #'(lambda (list)
						  (nthcdr number-of-elements list))
		 :collect (subseq tail 0 (min (length tail) number-of-elements)))))
    (partition file-lines 3)))

(defun group-common-items (elf-group)
  (priority-sum-of-group-common-items (convert-to-elf-groups (read-input-data "3.input")))
  (loop :for elf-rucksack :in elf-group
	:collect (item-set-from elf-rucksack) :into group-item-sets
	:finally (return (reduce #'intersection group-item-sets))))

(defun priority-sum-of-group-common-items (groups)
  (loop :for group :in groups
	:for common-item := (first (group-common-items group))
	:if (not (null common-item))
	  :collect (item-priority common-item) :into item-priorities
	:finally (return (reduce #'+ item-priorities))))

(priority-sum-of-group-common-items (convert-to-elf-groups (read-input-data "3.input")))
