(in-package :letcn)

;;; 3d equilateral simplex lattice
(defclass lattice (3d-object)
  ((lattice-values :initarg :lattice-values)))

;;; Cube consists of 6 simplices
;;; Edge traversal order from (0 0 0) to (1 1 1) identifies them
;;; Simplices have been ordered so that adjacent ones
;;; are next to each other in the array.
(defparameter *travel-orders*
  #(#(0 1 2)
    #(0 2 1)
    #(2 0 1)
    #(2 1 0)
    #(1 2 0)
    #(1 0 2)))

;;; Corner coordinates of unskewed cube
(defparameter *cube-corners*
  (let ((corners (make-array '(2 2 2) :initial-element nil)))
    (dotimes (i 2)
      (dotimes (j 2)
        (dotimes (k 2)
          (let* ((s (* unskew3d-factor (+ i j k)))
                 (x (- i s))
                 (y (- j s))
                 (z (- k s)))
            (setf (aref corners i j k) (make-vector x y z))))))
    corners))

;;; Simplex corner coordinates
(defparameter *simplex-corners*
  (let ((corners (make-array '(6 4) :initial-element nil)))
    (dotimes (simplex 6)
      (let ((v (make-array 3 :element-type 'bit :initial-element 0)))
        (setf (aref corners simplex 0) #(0.0 0.0 0.0))
        (loop with order = (aref *travel-orders* simplex)
              for corner from 1 to 3
              do (setf (aref v (aref order (1- corner))) 1)
              do (setf (aref corners simplex corner)
                       (aref *cube-corners*
                             (aref v 0)
                             (aref v 1)
                             (aref v 2))))))
    corners))

;;; Simplex midpoints
(defparameter *simplex-midpoints*
  (let ((midpoints (make-array 6 :initial-element nil)))
    (dotimes (simplex 6)
      (setf (aref midpoints simplex)
            (map 'vector (lambda (a b c d) (/ (+ a b c d) 4))
                 (aref *simplex-corners* simplex 0)
                 (aref *simplex-corners* simplex 1)
                 (aref *simplex-corners* simplex 2)
                 (aref *simplex-corners* simplex 3))))
    midpoints))

;;; Front faces are ccw
(defun simplex-faces (s)
  (if (evenp s)
    #2A((0 2 1) (0 1 3) (0 3 2) (1 2 3))
    #2A((0 1 2) (0 3 1) (0 2 3) (1 3 2))))

;;; Simplex face normals
(defparameter *simplex-normals*
  (let ((normals (make-array '(6 4) :initial-element nil)))
    (dotimes (simplex 6)
      (let ((faces (simplex-faces simplex)))
        (dotimes (f 4)
          (setf (aref normals simplex f)
                (normalize-vector (cross-product (map 'vector #'-
                                                      (aref *simplex-corners* simplex (aref faces f 1))
                                                      (aref *simplex-corners* simplex (aref faces f 0)))
                                                 (map 'vector #'-
                                                      (aref *simplex-corners* simplex (aref faces f 2))
                                                      (aref *simplex-corners* simplex (aref faces f 1)))))))))
    normals))

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
              (let ((midx (+ x (aref (aref *simplex-midpoints* simplex) 0)))
                    (midy (+ y (aref (aref *simplex-midpoints* simplex) 1)))
                    (midz (+ z (aref (aref *simplex-midpoints* simplex) 2))))
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

(defun make-test-lattice ()
  (let ((lattice-values (make-array (list 6 6 6 6)
                                    :element-type 'bit
                                    :initial-element 0)))
    (dotimes (i 6)
      (dotimes (j 6)
        (dotimes (k 6)
          (setf (aref lattice-values i j k (mod (+ i j k) 6)) 1))))
    (make-instance 'lattice :lattice-values lattice-values)))

(defmethod draw ((l lattice))
  (gl:enable :cull-face)
  (gl:front-face :ccw)
  (with-slots (lattice-values) l
    (dotimes (i (array-dimension lattice-values 0))
      (dotimes (j (array-dimension lattice-values 1))
        (dotimes (k (array-dimension lattice-values 2))
          (dotimes (simplex 6)
            (unless (zerop (aref lattice-values i j k simplex))
              (let* ((s (* unskew3d-factor (+ i j k)))
                     (x (- i s))
                     (y (- j s))
                     (z (- k s)))
                (let ((faces (simplex-faces simplex)))
                  (gl:with-primitives :triangles
                    (dotimes (a 4)
                      (let ((norm (aref *simplex-normals* simplex a)))
                        (gl:normal (aref norm 0)
                                   (aref norm 1)
                                   (aref norm 2)))
                      (dotimes (b 3)
                        (let ((v (aref *simplex-corners* simplex (aref faces a b))))
                          (gl:vertex (+ x (aref v 0))
                                     (+ y (aref v 1))
                                     (+ z (aref v 2))))))))))))))))
