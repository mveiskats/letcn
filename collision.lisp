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
