(in-package :letcn)

(defconstant 2pi (* 2 pi))
(defconstant pi/2 (/ pi 2))
(defconstant gl-float-size (cffi:foreign-type-size '%gl:float))

(defun coerce-vec (v)
  (map 'vec (lambda (a) (coerce a 'single-float)) v))

;;; Returns same angle between 0 and 2*pi
(defun normalize-angle (angle)
  (mod angle 2pi))

;;; Length of a vector squared
(defun vector-length-squared (v)
  (let ((x (aref v 0))
        (y (aref v 1))
        (z (aref v 2)))
    (+ (* x x) (* y y) (* z z))))

;;; Distance between two points squared
(defun distance-squared (p1 p2)
  (vector-length-squared (map 'vector #'- p1 p2)))

;;; Length of a vector
(defun vector-length (v)
  (sqrt (vector-length-squared v)))

;;; Distance between two points
(defun distance (p1 p2)
  (vec-length (vec- p1 p2)))

;;; Combine 3 vectors into a matrix
(defun vectors-to-matrix (v0 v1 v2)
  (matrix (aref v0 0) (aref v1 0) (aref v2 0) 0.0
          (aref v0 1) (aref v1 1) (aref v2 1) 0.0
          (aref v0 2) (aref v1 2) (aref v2 2) 0.0
          0.0 0.0 0.0 1.0))

;;; Traverses array by nested dotimes loops
(defmacro doarray (var-list arr &rest body)
  (labels ((dodimension (left-vars dimension)
             `(dotimes (,(car left-vars) (array-dimension ,arr ,dimension)) .
                ,(if (eq (cdr left-vars) nil) body
                   (list (dodimension (cdr left-vars) (1+ dimension)))))))
    (dodimension var-list 0)))
