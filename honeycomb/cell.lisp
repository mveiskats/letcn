(in-package :letcn)

(defconstant +troct-radius+
  (sqrt (+ (* 0.25 0.25) (* 0.5 0.5))))

;;; There must be a way to generate vertices and faces algorithmically
;;; Answer probably lies within combinatorics (see permutohedron)

(defvar +troct-vertices+
  ;; ordered in five slices of different z, starting from x=0, going ccw
  (let* ((a 0.25)
         (-a (- a))
         (2a (* a 2))
         (-2a (- 2a)))
    (make-array 24 :initial-contents
      (mapcar (lambda (v) (world-to-lattice (apply #'vec v)))
              `((0.0 ,a ,2a) (,-a 0.0 ,2a) (0.0 ,-a ,2a) (,a 0.0 ,2a)
                (0.0 ,2a ,a) (,-2a 0.0 ,a) (0.0 ,-2a ,a) (,2a 0.0 ,a)
                (,-a ,2a 0.0) (,-2a ,a 0.0) (,-2a ,-a 0.0) (,-a ,-2a 0.0) (,a ,-2a 0.0) (,2a ,-a 0.0) (,2a ,a 0.0) (,a ,2a 0.0)
                (0.0 ,2a ,-a) (,-2a 0.0 ,-a) (0.0 ,-2a ,-a) (,2a 0.0 ,-a)
                (0.0 ,a ,-2a) (,-a 0.0 ,-2a) (0.0 ,-a ,-2a) (,a 0.0 ,-2a))))))

;;; 3 vectors in lattice coordinates
;;; from which bounding rhombohedron will be generated
(defvar +cell-bounds+
  (loop for vert across +troct-vertices+
        maximize (aref vert 0) into max-x
        maximize (aref vert 1) into max-y
        maximize (aref vert 2) into max-z
        finally (return (list (vec max-x 0.0 0.0)
                              (vec 0.0 max-y 0.0)
                              (vec 0.0 0.0 max-z)))))

(defvar +troct-faces+
  ;; x = right, y = top, z = front/back
  (make-array 14 :initial-contents
    '((0 1 2 3)     ;; front
      (4 15 16 8)   ;; top
      (5 9 17 10)   ;; left
      (6 11 18 12)  ;; bottom
      (7 13 19 14)  ;; right
      (20 23 22 21) ;; back
      (0 3 7 14 15 4)     ;; right-top-front
      (20 16 15 14 19 23) ;; right-top-back
      (21 17 9 8 16 20)   ;; left-top-back
      (0 4 8 9 5 1)       ;; left-top-front
      (2 6 12 13 7 3)     ;; bottom-right-front
      (23 19 13 12 18 22) ;; bottom-right-back
      (22 18 11 10 17 21) ;; bottom-left-back
      (1 5 10 11 6 2))))   ;; bottom-left-front

(defvar +troct-normals+
  (map 'vector
       (lambda (f)
         (let ((v1 (lattice-to-world (aref +troct-vertices+ (first f))))
               (v2 (lattice-to-world (aref +troct-vertices+ (second f))))
               (v3 (lattice-to-world (aref +troct-vertices+ (third f)))))
           (world-to-lattice (normalize (cross-product (vec- v2 v1)
                                                       (vec- v3 v2))))))
       +troct-faces+))

;;; Midpoint of each face scaled by 2 is center of a neighbouring cell
(defvar +cell-neighbours+
  (labels ((vertex-sum (face)
             (reduce #'vec+
                    (mapcar (lambda (v) (aref +troct-vertices+ v)) face)
                    :initial-value (vec 0.0 0.0 0.0)))
           (neighbour-grid-offset (face)
             (map 'vector #'truncate (vec* (vertex-sum face)
                                           (/ 2.0 (length face))))))
    (map 'vector #'neighbour-grid-offset +troct-faces+)))

;;; Draw single face of truncated octahedron
(defun draw-troct-face (idx offset)
  (let ((face (aref +troct-faces+ idx))
        (normal (aref +troct-normals+ idx)))
    (gl:with-primitives :polygon
      (emit-normal normal)
      (dolist (v face)
        (emit-vertex (vec+ (aref +troct-vertices+ v) offset))))))

;;; Finds if line segment start-end intersects with troct centered on pos.
;;; Returns index of face closest to a or nil if there is no intersection
(defun line-troct-intersection (start end pos)
  (when (line-sphere-intersect? start end (coerce-vec pos) +troct-radius+)
    (let ((local-start (vec- start pos))
          (local-end (vec- end pos)))
      (block iteration
        (dotimes (i 14)
          ;; dont need backfacing polygons
          (when (and (> 0 (dot-product (vec- local-end local-start)
                                       (aref +troct-normals+ i)))
                     (let* ((face (aref +troct-faces+ i))
                            (v0 (aref +troct-vertices+ (first face)))
                            (v1 (aref +troct-vertices+ (second face)))
                            (v2 (aref +troct-vertices+ (third face)))
                            (inter (line-plane-intersection local-start
                                                            local-end
                                                            v1 v0 v2))
                            (p (aref inter 0))
                            (u (aref inter 1))
                            (v (aref inter 2)))
                         (and (not (eq p nil))
                              (<= 0 p 1)
                              (if (eq (length face) 4)
                                ;; square face
                                (and (<= 0 u 1)
                                     (<= 0 v 1))
                                ;; hexagon face
                                (and (<= 0 u 2)
                                     (<= 0 v 2)
                                     (<= (1- v) u (1+ v)))))))
            (return-from iteration i)))))))

(defun draw-highlight (center idx)
  (gl:color 0.5 0.0 0.0)
    (draw-troct-face idx center))

(defun emit-cell-color (i j k)
  (case (cell-value i j k)
    (1 (gl:color 0.7 0.3 0.3))
    (2 (gl:color 0.3 0.7 0.3))
    (t (gl:color 0.3 0.3 0.7))))

(defun draw-cell (i j k)
  (emit-cell-color i j k)
  (let ((center (coerce-vec (list i j k))))
    (dotimes (idx (length +troct-faces+))
      (multiple-value-bind (ii jj kk) (neighbour-cell i j k idx)
        (when (zerop (cell-value ii jj kk))
          (draw-troct-face idx center))))))
