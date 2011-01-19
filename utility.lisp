(in-package :letcn)

(defconstant 2pi (* 2 pi))
(defconstant pi/2 (/ pi 2))
(defconstant gl-float-size (cffi:foreign-type-size '%gl:float))

(defun make-matrix (contents)
  (make-array (list (length contents) (length (first contents)))
              :element-type 'float
              :initial-contents contents))

(defun make-vector (&rest contents)
  (make-array (list (length contents))
              :element-type 'float
              :initial-contents contents))

(defun vector+ (v1 v2 &optional (vector-type 'simple-vector))
  (map vector-type #'+ v1 v2))

(defun vector- (v1 v2 &optional (vector-type 'simple-vector))
  (map vector-type #'- v1 v2))

(defparameter identity-matrix
  (make-matrix '((1.0 0.0 0.0 0.0)
                 (0.0 1.0 0.0 0.0)
                 (0.0 0.0 1.0 0.0)
                 (0.0 0.0 0.0 1.0))))

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

;;; Matrix multiplication
(defun matrix-product (a b)
  (declare (array a b))
  (if (eq (array-rank b) 1)
    ;; matrix * vector
    (let* ((result (make-array 3 :initial-element 0)))
      (dotimes (i 3)
        (dotimes (k 3)
          (incf (aref result i)
                (* (aref a i k)
                   (aref b k)))))
      result)
    ;; matrix * matrix
    (let* ((m (array-dimension a 0))
           (n (array-dimension b 1))
           (result (make-array (list m n) :initial-element 0)))
      (dotimes (i m)
        (dotimes (j n)
          (dotimes (k (array-dimension a 1))
            (incf (aref result i j)
                  (* (aref a i k)
                     (aref b k j))))))
      result)))

;;; Axis has to be normalized vector
(defun rotation-matrix (axis angle)
  (let* ((x (aref axis 0))
         (y (aref axis 1))
         (z (aref axis 2))
         (sina (sin angle))
         (cosa (cos angle))
         (1-cosa (- 1 cosa)))
    (make-array
      '(4 4)
      :element-type 'float
      :initial-contents (list (list (+ cosa (* x x 1-cosa))
                                    (- (* x y 1-cosa) (* z sina))
                                    (+ (* x z 1-cosa) (* y sina))
                                    0.0)
                              (list (+ (* y x 1-cosa) (* z sina))
                                    (+ cosa (* y y 1-cosa))
                                    (- (* y z 1-cosa) (* x sina))
                                    0.0)
                              (list (- (* z x 1-cosa) (* y sina))
                                    (+ (* z y 1-cosa) (* x sina))
                                    (+ cosa (* z z 1-cosa))
                                    0.0)
                              '(0.0 0.0 0.0 1.0)))))

;;; Dot product of two vectors. Vectors must be of equal length
(defun dot-product (a b)
  (loop for i across a
        for j across b
        sum (* i j)))

(defconst skew3d-factor (/ 3.0))
(defconst unskew3d-factor (/ 6.0))

;;; Skews from unilateral simplexes to cubes
(defun skew3d-vector (v)
  (declare (type (vector float) v))
  (let ((s (* (loop for i across v sum i) skew3d-factor)))
    (map '(vector float) (lambda (i) (+ i s)) v)))

;;; Skews from cubes to unilateral simplexes
(defun unskew3d-vector (v)
  (declare (type (vector fixnum) v))
  (let ((s (/ (loop for i across v sum i) unskew3d-factor)))
    (map '(vector float) (lambda (i) (- i s)) v)))

