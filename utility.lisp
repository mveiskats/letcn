(in-package :letcn)
(import 'sb-cga::~)

(defconstant 2pi (* 2 pi))
(defconstant pi/2 (/ pi 2))
(defconstant float-size (cffi:foreign-type-size :float))

(defun coerce-vec (v)
  (map 'vec (lambda (a) (coerce a 'single-float)) v))

;;; Returns same angle between 0 and 2*pi
(defun normalize-angle (angle)
  (mod angle 2pi))

;;; Length of a vector squared
(defun vector-length-squared (v)
  (dot-product v v))

;;; Distance between two points squared
(defun distance-squared (p1 p2)
  (vector-length-squared (vec- p1 p2)))

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

(declaim (ftype (function (integer integer integer) integer) morton-order))
(defun morton-order (x y z)
  "Converts x y z to Morton order by interleaving their bits"
  (if (= 0 x y z) 0
    (multiple-value-bind (xquot xrem) (floor x 2)
      (multiple-value-bind (yquot yrem) (floor y 2)
        (multiple-value-bind (zquot zrem) (floor z 2)
          (+ (ash (morton-order xquot yquot zquot) 3)
             (ash xrem 2)
             (ash yrem 1)
             zrem))))))

(defun perspective-projection (fov-y aspect-ratio near far)
  (declare (single-float fov-y aspect-ratio near far))
  "Creates perspective projection matrix"
  (let ((f (/ (tan (* 0.5 fov-y)))))
    (matrix (/ f aspect-ratio) 0.0 0.0 0.0
            0.0 f 0.0 0.0
            0.0 0.0 (/ (+ far near) (- near far)) (/ (* 2 far near) (- near far))
            0.0 0.0 -1.0 0.0)))

(defun orthographic-projection (width height near far)
  (declare (fixnum width height near far))
  "Creates perspective projection matrix"
  (let ((1/r (coerce (/ 1 (/ width 2)) 'single-float))
        (1/t (coerce (/ 1 (/ height 2)) 'single-float))
        (2/n-f (coerce (/ 2 (- near far)) 'single-float))
        (f+n/n-f (coerce (/ (+ far near) (- near far)) 'single-float)))
    (matrix 1/r 0.0 0.0 0.0
            0.0 1/t 0.0 0.0
            0.0 0.0 2/n-f f+n/n-f
            0.0 0.0 0.0 1.0)))

(defun emit-vertex (v)
  "Calls gl:vertex"
  (gl:vertex (aref v 0) (aref v 1) (aref v 2)))

(defun emit-normal (n)
  "Calls gl:normal"
  (gl:normal (aref n 0) (aref n 1) (aref n 2)))

(defun deg-to-rad (deg)
  (coerce (* (/ pi 180) deg) 'single-float))

(defun rad-to-deg (rad)
  (coerce (* (/ 180 pi) rad) 'single-float))
