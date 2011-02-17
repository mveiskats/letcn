(in-package :letcn)

;;; 3d equilateral simplex lattice
(defclass lattice (3d-object)
  ((lattice-values :initarg :lattice-values)))

(defparameter *travel-orders*
  #(#(0 1 2)
    #(0 2 1)
    #(1 0 2)
    #(1 2 0)
    #(2 0 1)
    #(2 1 0)))

;;; Corner coordinates of cube that has been unskewed
(defparameter *simplex-offsets*
  (let ((offsets (make-array '(2 2 2 3) :element-type 'float)))
    (dotimes (i 2)
      (dotimes (j 2)
        (dotimes (k 2)
          (let* ((s (* unskew3d-factor (+ i j k)))
                 (x (- i s))
                 (y (- j s))
                 (z (- k s)))
            (setf (aref offsets i j k 0) x
                  (aref offsets i j k 1) y
                  (aref offsets i j k 2) z)))))
    offsets))

;;; base - base vertex
;;; i - travel order index
(defun simplex-vertices (base i)
  (let ((order (aref *travel-orders* i))
        (v (make-array 3 :element-type 'fixnum :initial-element 0))) 
    (cons (unskew3d-vector (map '(vector fixnum) #'+ base v))
          (loop for a across order
                do (incf (aref v a))
                collect (unskew3d-vector (map '(vector fixnum) #'+ base v))))))

(defun make-lattice (w)
  (let ((lattice-values (make-array (list w w w 6) :element-type 'bit)))
    (dotimes (x w)
      (dotimes (y w)
        (dotimes (z w)
          (dotimes (i 6)
            (let* ((o (aref *travel-orders* i))
                   ;; let there be corners
                   (c0 (make-array 3 :element-type 'fixnum
                                   :initial-contents (list x y z)))
                   c1 c2 c3)
              (setf c1 (copy-seq c0))
              (incf (aref c1 (aref o 0)))
              (setf c2 (copy-seq c1))
              (incf (aref c2 (aref o 1)))
              (setf c3 (copy-seq c2))
              (incf (aref c3 (aref o 2)))
              (let ((v (map '(vector float)
                            (lambda (a b c d)
                              (/ (+ a b c d) 4))
                            (unskew3d-vector c0)
                            (unskew3d-vector c1)
                            (unskew3d-vector c2)
                            (unskew3d-vector c3))))
              (setf (aref lattice-values x y z i)
                    (if (< (let* ((w/2 (/ w 2))
                                  (vx (- (aref v 0) w/2))
                                  (vy (- (aref v 1) w/2))
                                  (vz (- (aref v 2) w/2)))
                             (- (+ (* vx vx) (* vy vy) (* vz vz))
                                (* w/2 w/2)
                                -1.0))
                           (* 10 (noise3d-octaves (/ (aref v 0) 10)
                                                  (/ (aref v 1) 10)
                                                  (/ (aref v 2) 10)
                                                  3 0.25)))
                        1 0))))))))
    (make-instance 'lattice :lattice-values lattice-values)))

(defmethod draw ((l lattice))
  (with-slots (lattice-values) l
    (let ((v (make-array 3 :element-type 'fixnum)))
      (dotimes (x (array-dimension lattice-values 0))
        (dotimes (y (array-dimension lattice-values 1))
          (dotimes (z (array-dimension lattice-values 2))
            (dotimes (i 6)
              (unless (zerop (aref lattice-values x y z i))
                (setf (aref v 0) x
                      (aref v 1) y
                      (aref v 2) z)
                (let ((vertices (simplex-vertices v i)))
                  (gl:with-primitives :triangle-fan
                    (dolist (u vertices)
                      (gl:vertex (aref u 0) (aref u 1) (aref u 2)))
                    (let ((u (second vertices)))
                      (gl:vertex (aref u 0) (aref u 1) (aref u 2)))))))))))))
