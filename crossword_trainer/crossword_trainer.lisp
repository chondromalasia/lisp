(defpackage :crossword_trainer


  (:use :common-lisp))

(in-package crossword_trainer)

(ql:quickload :cl-csv)

; hacky, figure this out
(defvar clues (cl-csv:read-csv #P"~/Projects/lisp/crossword_trainer/test_csv.csv"))


(defun empty_clue (clue)
  (make-string (length clue) :initial-element #\□))

(defun helper_find_slots (x s)
  "Create list of index positions where a white square is present in a string s"
  (cond ((string= "" s) '())
	((eql #\□ (char s 0)) (cons x (helper_find_slots (+ x 1) (subseq s 1))))
	(t (helper_find_slots (+ x 1) (subseq s 1)))))

(defun find_slots (s)
  "Return a list of integars representing the index positions of character #\□
   in string s"
  (helper_find_slots 0 s))

(defun add_slot (x s1 s2)
  "At position x in view, overwrite with the corresponding character in s2
   note that this has global scope"
  (setf (char s1 x) (char s2 x)))

(defvar clue "test")
(defvar view (empty_clue clue))

(defun add_letter (view clue)
  (let ((new_char 0))
    (setq new_char (nth
		    (random (length (find_slots view)))
		    (find_slots view)))
    (setf (char view new_char) (char clue new_char))
  ))

(defun solve_clue (to_solve)
  (setf view (empty_clue to_solve))
  (loop while (find_slots view)
	do (setq input (read-line))
	   (cond
	     ((null (string= input to_solve))(add_letter view to_solve))
	     ((string= input to_solve) (setf view to_solve)))
	   (print view)
	)
  )
