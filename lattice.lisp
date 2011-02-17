(in-package :letcn)

;;; 3d equilateral simplex lattice
(defclass lattice (3d-object)
  ((lattice-values :initarg :lattice-values)))

;;; Cube consists of 6 simplices
;;; Edge traversal order from (0 0 0) to (1 1 1) identifies them
(defparameter *travel-orders*
  #(#(0 1 2)
    #(0 2 1)
    #(1 0 2)
    #(1 2 0)
    #(2 0 1)
    #(2 1 0)))

;;; Precomputed corner coordinates of cube that has been unskewed
(defparameter *simplex-corners*
  (let ((corners (make-array '(2 2 2 3)
                             :element-type 'float
                             :initial-element 0.0)))
    (dotimes (i 2)
      (dotimes (j 2)
        (dotimes (k 2)
          (let* ((s (* unskew3d-factor (+ i j k)))
                 (x (- i s))
                 (y (- j s))
                 (z (- k s)))
            (setf (aref corners i j k 0) x
                  (aref corners i j k 1) y
                  (aref corners i j k 2) z)))))
    corners))

;;; Precomputed simplex midpoints
(defparameter *simplex-midpoints*
  (let ((midpoints (make-array '(6 3)
                               :element-type 'float
                               :initial-element 0.0)))
    (dotimes (simplex 6)
      (let ((corner (make-array '(3) :element-type 'bit :initial-element 0)))
        (loop for o across (aref *travel-orders* simplex)
              do (setf (aref corner o) 1)
              do (dotimes (n 3)
                   (incf (aref midpoints simplex n)
                         (aref *simplex-corners*
                               (aref corner 0)
                               (aref corner 1)
                               (aref corner 2)
                               n))))
        (dotimes (n 3)
          (setf (aref midpoints simplex n)
                (/ (aref midpoints simplex n) 4)))))
    midpoints))

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
    (dotimes (i w)
      (dotimes (j w)
        (dotimes (k w)
          (let* ((s (* unskew3d-factor (+ i j k)))
                 (x (- i s))
                 (y (- j s))
                 (z (- k s)))
            (dotimes (simplex 6)
              (let ((midx (+ x (aref *simplex-midpoints* simplex 0)))
                    (midy (+ y (aref *simplex-midpoints* simplex 1)))
                    (midz (+ z (aref *simplex-midpoints* simplex 2))))
                (setf (aref lattice-values i j k simplex)
                      (if (< (let* ((w/2 (/ w 2))
                                    (vx (- midx w/2))
                                    (vy (- midy w/2))
                                    (vz (- midz w/2)))
                               (- (+ (* vx vx) (* vy vy) (* vz vz))
                                  (* w/2 w/2)
                                  -1.0))
                             (* 10 (noise3d-octaves (/ midx 10)
                                                    (/ midy 10)
                                                    (/ midz 10)
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
