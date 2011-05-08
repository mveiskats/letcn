(in-package :letcn)

;;; Detects if a line segment and sphere intersect
;;; a, b - endpoints of the line segment
;;; c, r - center and radius of the sphere
(defun line-sphere-intersect? (a b c r)
  (let* ((r-squared (* r r))
         (b-a (vector- b a))
         (c-a (vector- c a))
         (ac-squared (vector-length-squared c-a)))
    (or (>= r-squared ac-squared)
        (>= r-squared (distance-squared b c))
        (let ((dp (dot-product b-a c-a)))
          (and (> dp 0.0)
               (let* ((ab-squared (vector-length-squared b-a))
                      ;; d is the point on ab closest to c
                      (ad-squared (/ (* dp dp) ab-squared)))
                 (and (<= ac-squared ab-squared)
                      (<= (- ac-squared ad-squared) r-squared))))))))

;;; Determines intersection point of line (l0 + t(l1 - l0)) and
;;; plane (p0 + u(p1 - p0) + v(p2 - p0))
;;; returns t, u and v or nil if line and plane dont intersect
;;; !!! Note to self - do not attempt to bind to t
(defun line-plane-intersection (l0 l1 p0 p1 p2)
  (let ((result
         (matrix*vector (invert-matrix (vectors-to-matrix (vector- l0 l1)
                                                          (vector- p1 p0)
                                                          (vector- p2 p0)))
                        (vector- l0 p0))))
    (values (aref result 0) (aref result 1) (aref result 2))))
