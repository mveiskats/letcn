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
;;; corner - simplex corner coordinates (skewed)
;;; input-offset - corner offset from input point (not skewed)
;;; Note that only first 8 bits of each corner coordinate are used,
;;; making noise tile after 255
(defun gradient (corner input-offset)
  (declare (type (vector fixnum) corner)
           (type (vector float) input-offset))
  (let ((i (aref corner 0))
        (j (aref corner 1))
        (k (aref corner 2))
        (u (aref input-offset 0))
        (v (aref input-offset 1))
        (w (aref input-offset 2)))
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
        (+ p (if (zerop b01) (+ q r) (if b2 r q)))))))

;;; input is the point were generating noise for
;;; vv is one of the skewed simplex corners
(defun interpolation-kernel (input vv)
  (declare (type (vector float) input)
           (type (vector fixnum) vv))
  (let* ((u (map '(vector float) #'- input (unskew3d-vector vv)))
         (k (- 0.6 (loop for a across u sum (* a a)))))
    (if (<= k 0) 0 (* 8 k k k k (gradient vv u)))))

;;; Let's make some noise!
(defun noise3d (input)
  (declare (type (vector float) input))
  (let* ((ii (skew3d-vector input))
         (base-corner (map '(vector fixnum) #'floor ii))
         (skewed-offset (map '(vector float) #'- ii base-corner))
         (traverse-order (sort (make-vector 0 1 2)
                               #'> :key (lambda (i) (aref skewed-offset i)))))
    (+ (interpolation-kernel input base-corner)
       (loop for i across traverse-order
             do (incf (aref base-corner i))
             sum (interpolation-kernel input base-corner)))))

(defun convenient-noise (x y z)
  (noise3d (make-array 3 :element-type 'float
                       :initial-contents (list (float x) (float y) (float z)))))

(defun noise3d-octaves (x y z octaves persistence)
  (if (> octaves 0)
    (* persistence
       (+ (convenient-noise x y z)
          (noise3d-octaves (* x 2.0) (* y 2.0) (* z 2.0)
                           (1- octaves) persistence)))
    0))

;;;;
;;;; For noise visualization purposes
;;;;

(defclass grid (3d-object)
  ((vertices :initarg :vertices)))

(defun make-grid ()
  (let ((vertices (make-array '(300 300 3) :element-type 'float))
        (tmp (make-array '(3) :element-type 'float :initial-element 0.0))
        (tmp2 (make-array '(3) :element-type 'float :initial-element 0.0)))
    (dotimes (i 300)
      (dotimes (j 300)
        (let ((x (/ (- i 150) 10))
              (y (/ (- j 150) 10)))
          (setf (aref tmp 0) (/ x 5.0)
                (aref tmp 1) (/ y 5.0)
                (aref tmp2 0) (float x)
                (aref tmp2 1) (float y)
                (aref vertices i j 0) (float x)
                (aref vertices i j 1) (float y)
                (aref vertices i j 2) (+ (* 5 (noise3d tmp))
                                         (noise3d tmp2))))))
    (make-instance 'grid :vertices vertices)))

(defmethod draw ((g grid))
  (with-slots (vertices) g
    (dotimes (i (array-dimension vertices 0))
      (gl:with-primitives :line-strip
        (dotimes (j (array-dimension vertices 1))
          (gl:vertex (aref vertices i j 0) (aref vertices i j 1) (aref vertices i j 2)))))
    (dotimes (j (array-dimension vertices 1))
      (gl:with-primitives :line-strip
        (dotimes (i (array-dimension vertices 0))
          (gl:vertex (aref vertices i j 0) (aref vertices i j 1) (aref vertices i j 2)))))))
