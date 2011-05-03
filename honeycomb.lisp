(in-package :letcn)

(defparameter *troct-radius*
  (sqrt (+ (* 0.25 0.25) (* 0.5 0.5))))

(defclass honeycomb (3d-object)
  ((cell-values :initarg :cell-values)))

;;; Draw truncated octahedron
(defun draw-troct ()
  (let* ((a 0.25)
         (-a (- a))
         (2a (* a 2))
         (-2a (- 2a))
         ;; five slices of different z, starting from x=0, going ccw
         (vertices `((0.0 ,a ,2a) (,-a 0.0 ,2a) (0.0 ,-a ,2a) (,a 0.0 ,2a)
                     (0.0 ,2a ,a) (,-2a 0.0 ,a) (0.0 ,-2a ,a) (,2a 0.0 ,a)
                     (,-a ,2a 0.0) (,-2a ,a 0.0) (,-2a ,-a 0.0) (,-a ,-2a 0.0) (,a ,-2a 0.0) (,2a ,-a 0.0) (,2a ,a 0.0) (,a ,2a 0.0)
                     (0.0 ,2a ,-a) (,-2a 0.0 ,-a) (0.0 ,-2a ,-a) (,2a 0.0 ,-a)
                     (0.0 ,a ,-2a) (,-a 0.0 ,-2a) (0.0 ,-a ,-2a) (,a 0.0 ,-2a))))
    (let (;; x = right, y = top, z = front/back
          (faces '((0 1 2 3)     ;; front
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
                   (1 5 10 11 6 2)     ;; bottom-left-front
                   )))
      (let* ((b (sqrt 1/3))
             (-b (- b))
             (normals `(( 0.0  0.0  1.0) ;; front
                        ( 0.0  1.0  0.0) ;; top
                        (-1.0  0.0  0.0) ;; left
                        ( 0.0 -1.0  0.0) ;; bottom
                        ( 1.0  0.0  0.0) ;; right
                        ( 0.0  0.0  1.0) ;; back
                        ( ,b  ,b  ,b) ;; right-top-front
                        ( ,b  ,b ,-b) ;; right-top-back
                        (,-b  ,b ,-b) ;; left-top-back
                        (,-b  ,b  ,b) ;; left-top-front
                        ( ,b ,-b  ,b) ;; right-bottom-front
                        ( ,b ,-b ,-b) ;; right-bottom-back
                        (,-b ,-b ,-b) ;; left-bottom-back
                        (,-b ,-b  ,b) ;; left-bottom-front
                        )))
             (loop for f in faces
                   for n in normals
                   do (gl:with-primitives :polygon
                        (apply #'gl:normal n)
                        (loop for v in f
                              do (apply #'gl:vertex (nth v vertices)))))))))

(let* ((g2p #2A((0.5 0.0 0.5)
                (0.5 1.0 0.5)
                (0.5 0.0 -0.5)))
       (p2g (invert-matrix g2p)))
  (defun grid-to-pos (g)
    (matrix-product g2p g))
  (defun pos-to-grid (p)
    (map 'vector #'truncate (matrix-product p2g p))))

(defun make-honeycomb (size)
  (let ((result (make-array (list size size size)
                            :element-type 'bit
                            :initial-element 0)))
    (dotimes (i size)
      (dotimes (j size)
        (dotimes (k size)
          (let ((p (grid-to-pos (make-vector i j k))))
            (if (> 0 (* 10 (noise3d-octaves (/ (aref p 0) 10)
                                            (/ (aref p 1) 10)
                                            (/ (aref p 2) 10)
                                            3 0.25)))
                (setf (aref result i j k) 1))))))
    (make-instance 'honeycomb :cell-values result)))

(defmethod draw ((hc honeycomb))
  (with-slots (cell-values) hc
    (dotimes (i (array-dimension cell-values 0))
      (dotimes (j (array-dimension cell-values 1))
        (dotimes (k (array-dimension cell-values 2))
          (unless (zerop (aref cell-values i j k))
            (let ((p (grid-to-pos (make-vector i j k))))
              (gl:with-pushed-matrix
                (gl:translate (aref p 0) (aref p 1) (aref p 2))
                (draw-troct)))))))))

;;; Step through cubic lattice (edge length 0.5) cell by cell.
;;; Each cell is shared by exactly 2 cells of the honeycomb.
;;; Keep last 4 visited honeycomb cells in buffer to avoid testing
;;; same cell twice and to insure they are visited in proper order.
;;; This probably is not the most efficient way, but it's good enough.
(defun rasterize-honeycomb (start end callback)
  (let ((start*2 (vector* start 2))
        (end*2 (vector* end 2))
        (buffer nil))
    (flet ((push-cell (c)
             (unless (find-if (lambda (a) (equal (car a) c)) buffer)
               (push (cons c (distance-squared start*2 c)) buffer)))
           (pop-cell ()
             (apply callback (mapcar (lambda (a) (* a 0.5)) (caar buffer)))
             (setf buffer (cdr buffer))))
    (rasterize (aref start*2 0) (aref start*2 1) (aref start*2 2) 
               (aref end*2 0) (aref end*2 1) (aref end*2 2)
      (lambda (i j k)
        (let ((even-cell (list (if (evenp i) i (1+ i))
                               (if (evenp j) j (1+ j))
                               (if (evenp k) k (1+ k))))
              (odd-cell (list (if (oddp i) i (1+ i))
                              (if (oddp j) j (1+ j))
                              (if (oddp k) k (1+ k)))))
            (push-cell even-cell)
            (push-cell odd-cell)
            (setf buffer (sort buffer #'< :key #'cdr))
            (loop while (> (length buffer) 4)
                  do (pop-cell)))))
    (loop while buffer
          do (pop-cell)))))

(defun find-closest-hit (a b hc)
  (block stepper 
    (with-slots (cell-values) hc
      (rasterize-honeycomb a b
        (lambda (x y z)
          (let* ((pos (make-vector x y z))
                 (cell (pos-to-grid pos))
                 (i (aref cell 0))
                 (j (aref cell 1))
                 (k (aref cell 2)))
            (when (and (< -1 i (array-dimension cell-values 0))
                       (< -1 j (array-dimension cell-values 1))
                       (< -1 k (array-dimension cell-values 2))
                       (not (zerop (aref cell-values i j k)))
                       (line-sphere-intersect? a b pos *troct-radius*))
              (return-from stepper pos))))))))

(defun draw-highlight (c)
  (gl:color 0.5 0.0 0.0)
  (gl:with-pushed-matrix
    (gl:translate (aref c 0) (aref c 1) (aref c 2))
    (draw-troct)))