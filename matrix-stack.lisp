(in-package :letcn)

(defvar *transformation* +identity-matrix+)

(defun get-transformation ()
  *transformation*)

(defmacro with-transformation (mat &rest body)
  `(let ((*transformation* (matrix* (get-transformation) ,mat)))
     ,@body))
