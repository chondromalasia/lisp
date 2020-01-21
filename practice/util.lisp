(make-package :util :use '(:cl))


(in-package util)

(defun elementp (o)
  (or
   (symbolp o)
   (characterp o)
   (numberp o)
   (packagep o)))

(export elementp)


