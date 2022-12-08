;;
;; Notations:
;;
;; A -- Rock
;; B -- Paper
;; C -- Scissor
;;
;; X -- Rock
;; Y -- Paper
;; Z -- Scissor

;; A = X
;; B = Y
;; C = Z
;;
;; A > Z
;; B > X
;; C > Y

;;
;; Score points:
;;
;; Rock = 1
;; Paper = 2
;; Scissor = 3
;;
;; Win = 6
;; Draw = 3
;; Loss = 0
;;

;;   |  X  Y  Z
;;---+---------
;; A |  0  1  2
;; B | -1  0  1
;; C | -2 -1  0 
;;

;; Part 1
(defconstant +score-points+ '((A 1) (X 1)
			      (B 2) (Y 2)
			      (C 3) (Z 3)
			      (win 6) (draw 3) (loss 0)))

(defconstant +win-values+  '(1 -2))

(defun play (elf-choice player-choice)
  (let* ((elf-choice-value (cadr (assoc elf-choice +score-points+)))
	 (player-choice-value (cadr (assoc player-choice +score-points+)))
	 (game-result (- player-choice-value
			 elf-choice-value)))
    (cond
      ((zerop game-result) 'draw)
      ((find game-result +win-values+) 'win)
      (t 'loss))))
      
(defun calculate-score (player-choice game-result)
  (+ (cadr (assoc player-choice +score-points+))
     (cadr (assoc game-result +score-points+))))

(defun read-input (filepath)
  (with-open-file (input-stream filepath :direction :input)
    (loop :for line := (read-line input-stream nil)
	  :while line
	  :collect line)))
    
(defun convert-input-data (file-lines)
  (labels ((split (seq &optional (delimiter " ") (test #'string-equal))
	     (let ((delimiter-position (position delimiter seq :test test)))
	       (cond
		 ((null delimiter-position) (list seq))
		 (t (cons (subseq seq 0 delimiter-position)
			  (split (subseq seq (1+ delimiter-position)))))))))
    (loop :for line :in file-lines
	  :collect (mapcar #'intern (split line)))))

(defun calculate-total-score (filepath)
  (let ((game-rounds (convert-input-data (read-input filepath))))
    (loop :for (elf-choice player-choice) :in game-rounds
	  :sum (calculate-score player-choice (play elf-choice player-choice)))))


;; Part 2
;;
;; X -- Loss
;; Y -- Draw
;; Z -- Win
;;

(defconstant +choice-priority-list+ '(A B C)); circular from left to right, A loses to B and B loses to C and C loses to A
(defun infer-choice (elf-choice game-outcome)
  (labels ((choose (direction)
	     (nth (mod (funcall direction (position elf-choice +choice-priority-list+))
		       (length +choice-priority-list+))
		  +choice-priority-list+))
	   (choose-next () (choose #'1+))
	   (choose-previous () (choose #'1-)))
    (cond
      ((equal game-outcome 'win) (choose-next))
      ((equal game-outcome 'loss) (choose-previous))
      (t elf-choice))))


(defconstant +game-outcome-assoc+ '((X loss)
				    (Y draw)
				    (Z win)))
(defun calculate-total-score (filepath)
  (let ((game-rounds (convert-input-data (read-input filepath))))
    (loop :for (elf-choice game-outcome-letter) :in game-rounds
	  :for game-outcome := (cadr (assoc game-outcome-letter +game-outcome-assoc+))
	  :sum (calculate-score (infer-choice elf-choice game-outcome) game-outcome))))
