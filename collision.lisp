(in-package :letcn)

;;; Detects if a line segment and sphere intersect
;;; a, b - endpoints of the line segment
;;; c, r - center and radius of the sphere
(defun line-sphere-intersect? (a b c r)
  (let* ((r-squared (* r r))
         (b-a (vec- b a))
         (c-a (vec- c a))
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
  (transform-direction (vec- l0 p0)
                       (inverse-matrix (vectors-to-matrix (vec- l0 l1)
                                                          (vec- p1 p0)
                                                          (vec- p2 p0)))))

;;; Returns point on line segment that intersects the specified sphere.
;;; Returns a if it is within the sphere.
;;; If there are two intersections, returns one closest to a.
;;; a, b - endpoints of the line segment
;;; c, r - center and radius of the sphere
;;; for details, see line-sphere intersection on wikipedia
(defun line-sphere-intersection (a b c r)
  ;; Translate everything so that a is at (0 0 0)
  (let ((b-a (vec- b a))
        (c-a (vec- c a)))
    (if (<= (dot-product c-a c-a) r)
        a ;; Starting point is within the sphere
        (let* ((bc (dot-product b-a c-a))
               (b^2 (dot-product b-a b-a))
               ;; determinant of quadratic equation
               (d (- (* bc bc) (* b^2 (- (dot-product c-a c-a) (* r r)))))
               (p (if (< d 0.0)
                      nil ;; Line doesnt intersect sphere
                      (let* ((sqrt-d (sqrt d))
                             (p1 (/ (+ bc sqrt-d) b^2))
                             (p2 (/ (- bc sqrt-d) b^2))
                             (p1-in-segment (<= 0.0 p1 1.0))
                             (p2-in-segment (<= 0.0 p2 1.0)))
                        (cond ((and p1-in-segment p2-in-segment) (min p1 p2))
                              (p1-in-segment p1)
                              (p2-in-segment p2)
                              (t nil))))))
          (if p (vec+ a (vec* b-a p)) nil)))))
