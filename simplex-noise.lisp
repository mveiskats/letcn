(in-package :letcn)

;;; used to determine pseudorandom gradient in simplex noise
(defun shuffle (i j k b)
  (declare (integer i j k)
           (unsigned-byte b))
  (aref #(#x15 #x38 #x32 #x2c #x0d #x13 #x07 #x2a)
        (+ (if (logbitp b i) 4 0)
           (if (logbitp b j) 2 0)
           (if (logbitp b k) 1 0))))

;;; Coz Perlin says so
;;; i j k - simplex corner coordinates (skewed)
;;; u v w - corner offset from input point (not skewed)
;;; Note that only first 8 bits of each corner coordinate are used,
;;; making noise tile after 255
(defun gradient (i j k u v w)
  (declare (fixnum i j k)
           (float u v w))
  (let ((index (+ (shuffle i j k 0)
                  (shuffle j k i 1)
                  (shuffle k i j 2)
                  (shuffle i j k 3)
                  (shuffle j k i 4)
                  (shuffle k i j 5)
                  (shuffle i j k 6)
                  (shuffle j k i 7))))
    (let ((b01 (logand #b11 index))
          (b2 (logbitp 2 index))
          (b3 (logbitp 3 index))
          (b4 (logbitp 4 index))
          (b5 (logbitp 5 index))
          p q r)
      (case b01
        (1 (setf p u q v r w))
        (2 (setf p v q w r u))
        (t (setf p w q u r v)))
      (when (eq b5 b3) (setf p (- p)))
      (when (eq b5 b4) (setf q (- q)))
      (when (eq b5 (not (eq b4 b3 ))) (setf r (- r)))
      (+ p (if (zerop b01) (+ q r) (if b2 r q))))))

;;; x y z are the coordinates were generating noise for
;;; i j k is one of the skewed simplex corners
(defun interpolation-kernel (x y z i j k)
  (declare (float x y z)
           (fixnum i j k))
  (let* ((s (* unskew3d-factor (+ i j k)))
         (u (- x (- i s)))
         (v (- y (- j s)))
         (w (- z (- k s)))
         (q (- 0.6 (+ (* u u) (* v v) (* w w)))))
    (if (<= q 0) 0 (* 8 q q q q (gradient i j k u v w)))))

(defun traverse-order (x y z)
  (declare (float x y z))
  (sort (list 0 1 2) #'> :key (lambda (i) (case i (0 x) (1 y) (2 z)))))

;;; Let's make some noise!
(defun noise3d (x y z)
  (declare (float x y z))
  (let ((s (* skew3d-factor (+ x y z)))
        i j k
        u v w)
    (multiple-value-setq (i u) (floor (+ x s)))
    (multiple-value-setq (j v) (floor (+ y s)))
    (multiple-value-setq (k w) (floor (+ z s)))
    (let ((corner (make-array 3 :element-type 'fixnum
                              :initial-contents (list i j k))))
      (+ (interpolation-kernel x y z
                               (aref corner 0)
                               (aref corner 1)
                               (aref corner 2))
         (loop for axis in (traverse-order u v w)
               do (incf (aref corner axis))
               sum (interpolation-kernel x y z
                                         (aref corner 0)
                                         (aref corner 1)
                                         (aref corner 2)))))))

(defun noise3d-octaves (x y z octaves persistence)
  (if (> octaves 0)
    (* persistence
       (+ (noise3d x y z)
          (noise3d-octaves (* x 2.0) (* y 2.0) (* z 2.0)
                           (1- octaves) persistence)))
    0))
