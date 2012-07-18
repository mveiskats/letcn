(in-package :letcn)

;;;; Most of the operations here only work with unit quaternions.
;;;; Normalizing quaternions is left to the caller.

(deftype quat ()
  "Quaternion"
  '(simple-array single-float (4)))

(declaim (ftype (function (single-float single-float single-float single-float)
                          quat)
                quat))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun quat (a b c d)
    (make-array 4
                :element-type 'single-float
                :initial-contents (list a b c d))))

(defconstant +identity-quat+
  (if (boundp '+identity-quat+)
      (symbol-value '+identity-quat+)
      (quat 1.0 0.0 0.0 0.0))
  "Constant identity quaternion")

(defun normalize-quat (q)
  (macrolet ((refsq (i) `(let ((a (aref q ,i))) (* a a))))
    (let ((len-squared (+ (refsq 0) (refsq 1) (refsq 2) (refsq 3))))
      (if (~ len-squared 1.0) q
        (let ((len (sqrt len-squared)))
          (quat (/ (aref q 0) len)
                (/ (aref q 1) len)
                (/ (aref q 2) len)
                (/ (aref q 3) len)))))))

(defun quat* (v w)
  "Quaternion multiplication"
  (if (eq v +identity-quat+) w
    (if (eq w +identity-quat+) v
      (let ((v0 (aref v 0))
            (v1 (aref v 1))
            (v2 (aref v 2))
            (v3 (aref v 3))
            (w0 (aref w 0))
            (w1 (aref w 1))
            (w2 (aref w 2))
            (w3 (aref w 3)))
        (quat (- (* v0 w0) (* v1 w1) (* v2 w2) (* v3 w3))
              (+ (* v0 w1) (* v1 w0) (* v2 w3) (- (* v3 w2)))
              (+ (* v0 w2) (- (* v1 w3)) (* v2 w0) (* v3 w1))
              (+ (* v0 w3) (* v1 w2) (- (* v2 w1)) (* v3 w0)))))))

(defun quat-to-matrix (q)
  "Constructs rotation matrix from quaternion"
  (let* ((a (aref q 0))
         (b (aref q 1))
         (c (aref q 2))
         (d (aref q 3))
         (aa (* a a))
         (bb (* b b))
         (cc (* c c))
         (dd (* d d))
         (2ab (* 2 a b))
         (2ac (* 2 a c))
         (2ad (* 2 a d))
         (2bc (* 2 b c))
         (2bd (* 2 b d))
         (2cd (* 2 c d)))
    (matrix (- (+ aa bb) (+ cc dd)) (- 2bc 2ad) (+ 2bd 2ac) 0.0
            (+ 2bc 2ad) (- (+ aa cc) (+ bb dd)) (- 2cd 2ab) 0.0
            (- 2bd 2ac) (+ 2cd 2ab) (- (+ aa dd) (+ bb cc)) 0.0
            0.0 0.0 0.0 1.0)))

(declaim (ftype (function (vec single-float) quat) axis-angle-to-quat))
(defun axis-angle-to-quat (axis angle)
  "Constructs quaternion from axis unit vector and angle"
  (let ((sa (sin angle)))
    (quat (cos angle)
          (* (aref axis 0) sa)
          (* (aref axis 1) sa)
          (* (aref axis 2) sa))))

(defun quat-inverse (q)
  "Creates inverse of unit quaternion"
  (quat (aref q 0)
        (- (aref q 1))
        (- (aref q 2))
        (- (aref q 3))))

(defun quat-rotate (v q)
  "Applies rotation of quaternion q to vector v"
  (let* ((tmp (quat 0.0 (aref v 0) (aref v 1) (aref v 2)))
         (result (quat* (quat* q tmp) (quat-inverse q))))
    (vec (aref result 1) (aref result 2) (aref result 3))))

;;(defun quat-slerp (q0 q1 p))
