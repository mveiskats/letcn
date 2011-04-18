(in-package :letcn)

(defun make-matrix (contents)
  (make-array (list (length contents) (length (first contents)))
              :element-type 'float
              :initial-contents contents))

(defparameter identity-matrix
  (make-matrix '((1.0 0.0 0.0 0.0)
                 (0.0 1.0 0.0 0.0)
                 (0.0 0.0 1.0 0.0)
                 (0.0 0.0 0.0 1.0))))

;;; Calculate inverse matrix. Returns nil if not inversible.
(defun invert-matrix (m)
  (declare (type (array float (3 3)) m))
  (let ((m00 (aref m 0 0)) (m01 (aref m 0 1)) (m02 (aref m 0 2))
        (m10 (aref m 1 0)) (m11 (aref m 1 1)) (m12 (aref m 1 2))
        (m20 (aref m 2 0)) (m21 (aref m 2 1)) (m22 (aref m 2 2)))
    (let ((det3 (- (+ (* m00 m11 m22) (* m10 m21 m02) (* m20 m01 m12))
                   (* m00 m21 m12) (* m10 m01 m22) (* m20 m11 m02))))
      (unless (zerop det3)
        (flet ((det2/det3 (a b c d) (/ (- (* a d) (* b c)) det3)))
          (make-matrix (list (list (det2/det3 m11 m12 m21 m22)
                                   (det2/det3 m02 m01 m22 m21)
                                   (det2/det3 m01 m02 m11 m12))
                             (list (det2/det3 m12 m10 m22 m20)
                                   (det2/det3 m00 m02 m20 m22)
                                   (det2/det3 m02 m00 m12 m10))
                             (list (det2/det3 m10 m11 m20 m21)
                                   (det2/det3 m01 m00 m21 m20)
                                   (det2/det3 m00 m01 m10 m11)))))))))

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