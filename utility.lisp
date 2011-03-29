(in-package :letcn)

(defconstant 2pi (* 2 pi))
(defconstant pi/2 (/ pi 2))
(defconstant gl-float-size (cffi:foreign-type-size '%gl:float))

(defun make-vector (&rest contents)
  (make-array (list (length contents))
              :element-type 'float
              :initial-contents contents))

(defun vector+ (v1 v2 &optional (vector-type 'simple-vector))
  (map vector-type #'+ v1 v2))

(defun vector- (v1 v2 &optional (vector-type 'simple-vector))
  (map vector-type #'- v1 v2))

(defun draw-triangle-list (tri-list)
  (declare (sequence tri-list))
  (gl:with-primitives :triangles
    (dolist (triangle tri-list)
      (dolist (vertex triangle)
        (apply #'gl:normal vertex)
        (apply #'gl:vertex vertex)))))

;;; Returns same angle between 0 and 2*pi
(defun normalize-angle (angle)
  (mod angle 2pi))

;;; Returns length of a vector
(defun vector-length (v)
  (let ((x (aref v 0))
        (y (aref v 1))
        (z (aref v 2)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

;;; Returns distance between two points
(defun distance (p1 p2)
  (vector-length (map 'vector #'- p1 p2)))

;;; Makes parallel vector with length of 1
(defun normalize-vector (v)
  (let ((l (vector-length v)))
    (map 'vector (lambda (a) (/ a l)) v)))

;;; Dot product of two vectors. Vectors must be of equal length
(defun dot-product (a b)
  (loop for i across a
        for j across b
        sum (* i j)))

;;; Cross product of two vectors.
(defun cross-product (a b)
  (declare (type (vector float 3)))
  (make-array 3
              :element-type 'float
              :initial-contents (list (- (* (aref a 1) (aref b 2))
                                         (* (aref a 2) (aref b 1)))
                                      (- (* (aref a 2) (aref b 0))
                                         (* (aref a 0) (aref b 2)))
                                      (- (* (aref a 0) (aref b 1))
                                         (* (aref a 1) (aref b 0))))))

(defparameter skew3d-factor (/ 3.0))
(defparameter unskew3d-factor (/ 6.0))

;;; Skews from unilateral simplexes to cubes
(defun skew3d-vector (v)
  (declare (type (vector float) v))
  (let ((s (* (loop for i across v sum i) skew3d-factor)))
    (map '(vector float) (lambda (i) (+ i s)) v)))

;;; Skews from cubes to unilateral simplexes
(defun unskew3d-vector (v)
  (declare (type (vector fixnum) v))
  (let ((s (* (loop for i across v sum i) unskew3d-factor)))
    (map '(vector float) (lambda (i) (- i s)) v)))

