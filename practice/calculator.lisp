(make-package :calculator :use '(:cl))

(in-package calculator)

(defun combine-expr (ex n l)
  ;(list ex n l)
  ;(list (list n ex (first l))) ( second l) (list (third l) (fourth l)))
  (list (list n ex (first l)) (second l) (list (third l) (fourth l) (fifth l))))

