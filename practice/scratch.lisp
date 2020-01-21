
;;; get the last element of a list
(defun get_last (a_list)
  (nth (- (length a_list) 1) a_list )
  )
