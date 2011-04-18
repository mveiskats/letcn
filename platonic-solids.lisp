(in-package :letcn)

;;; Converts from spherical coordinates to cartesian
(defun spherical2cartesian (a b radius)
  (let* ((x (* radius (cos a) (sin b)))
         (y (* radius (sin a) (sin b)))
         (z (* radius (cos b))))
    (list x y z)))

(defun make-tetrahedron (radius)
  (let* ((2/3pi (coerce (* 2/3 pi) 'single-float))
         (dihedral-angle (acos 1/3))
         (theta (coerce (- pi dihedral-angle) 'single-float))
         (a (spherical2cartesian 0 0 radius))
         (b (spherical2cartesian 0 theta radius))
         (c (spherical2cartesian 2/3pi theta radius))
         (d (spherical2cartesian (- 2/3pi) theta radius)))
    (list (list a c b)
          (list a b d)
          (list a d c)
          (list b c d))))

(defun make-icosahedron (radius)
  (let* ((phi (/ (+ 1 (sqrt 5)) 2))
         (a (/ radius (sqrt (+ 1 (* phi phi))))) ; shorter side of rectangle
         (b (* a phi)) ; longer side of rectangle
         (-a (- a))
         (-b (- b))
         (vertices `((,-a 0.0 ,b) (,a 0.0 ,b) (,-a 0.0 ,-b) (,a 0.0 ,-b)
                     (0.0 ,b ,a) (0.0 ,b ,-a) (0.0 ,-b ,a) (0.0 ,-b ,-a)
                     (,b ,a 0.0) (,-b ,a 0.0) (,b ,-a 0.0) (,-b ,-a 0.0)))
         (indices '((0 4 1) (0 9 4) (9 5 4) (4 5 8) (4 8 1)
                    (8 10 1) (8 3 10) (5 3 8) (5 2 3) (2 7 3)
                    (7 10 3) (7 6 10) (7 11 6) (11 0 6) (0 1 6)
                    (6 1 10) (9 0 11) (9 11 2) (9 2 5) (7 2 11))))
    (loop
       for tri in indices
       collect (list (nth (first tri) vertices)
                     (nth (second tri) vertices)
                     (nth (third tri) vertices)))))
