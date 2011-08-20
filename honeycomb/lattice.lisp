(in-package :letcn)

(defvar +lod-distance-squared+ 1024)

;;; Transformations between world position
;;; and lattice coordinates of cell
(defvar +l2w-transform+
  (matrix 0.5  0.0  0.5  0.0
          0.5  1.0  0.5  0.0
          0.5  0.0 -0.5  0.0
          0.0  0.0  0.0  1.0))

(defvar +w2l-transform+
  (inverse-matrix +l2w-transform+))

(defun lattice-to-world (g) (transform-direction g +l2w-transform+))
(defun world-to-lattice (w) (transform-direction w +w2l-transform+))
(defun world-to-lattice-int (w) (map 'vector #'truncate (world-to-lattice w)))

;;; Determines indices of a neighbour cell
(defun neighbour-cell (i j k face)
  (let ((neighbour (aref +cell-neighbours+ face)))
    (values (+ i (aref neighbour 0))
            (+ j (aref neighbour 1))
            (+ k (aref neighbour 2)))))

(defun rasterize (x0 y0 z0 x1 y1 z1 callback)
  (let ((dx (abs (- x1 x0)))
        (dy (abs (- y1 y0)))
        (dz (abs (- z1 z0))))
    (let ((swap-xy (and (> dy dx) (>= dy dz)))
          (swap-xz (and (> dz dx) (> dz dy))))
      ;; Note for optimization - only one of these is true
      (when swap-xy
        (rotatef x0 y0)
        (rotatef x1 y1)
        (rotatef dx dy))
      (when swap-xz
        (rotatef x0 z0)
        (rotatef x1 z1)
        (rotatef dx dz))
      (let* ((step-x (if (> x1 x0) 1 -1))
             (step-y (if (> y1 y0) 1 -1))
             (step-z (if (> z1 z0) 1 -1))
             (x (floor x0))
             (y (floor y0))
             (z (floor z0))
             (dy/dx (/ dy dx))
             (dz/dx (/ dz dx))
             (drift-x (if (> x1 x0) (- x0 (floor x0)) (- (ceiling x0) x0)))
             (drift-y (if (> y1 y0)
                        (- (- y0 (floor y)) (* dy/dx drift-x))
                        (- (- (ceiling y0) y0) (* dy/dx drift-x))))
             (drift-z (if (> z1 z0)
                        (- (- z0 (floor z)) (* dz/dx drift-x))
                        (- (- (ceiling z0) z0) (* dz/dx drift-x)))))
        (labels ((swap-and-callback ()
                   (let ((xx x) (yy y) (zz z))
                     (when swap-xz (rotatef xx zz))
                     (when swap-xy (rotatef xx yy))
                     (funcall callback xx yy zz)))
                 (inc-y ()
                   (incf y step-y)
                   (unless (= drift-y 1)
                     (swap-and-callback))
                   (decf drift-y))
                 (inc-z ()
                   (incf z step-z)
                   (unless (= drift-z 1)
                     (swap-and-callback))
                   (decf drift-z)))
          (loop do (swap-and-callback)
                do (incf drift-y dy/dx)
                do (incf drift-z dz/dx)
                do (if (and (>= drift-y 1) (>= drift-z 1))
                     ;; Special case - both y and z change in the same step
                     ;; so we have to determine which one goes first
                     (if (> (/ (- drift-y 1) dy/dx)
                            (/ (- drift-z 1) dz/dx))
                       (progn (inc-y) (inc-z))
                       (progn (inc-z) (inc-y)))
                     (if (>= drift-y 1)
                       (inc-y)
                       (if (>= drift-z 1)
                         (inc-z))))
                do (incf x step-x)
                while (if (> step-x 0) (< x x1) (> (1+ x) x1))))))))


;;; Step through cubic lattice (edge length 0.5) cell by cell.
;;; Each cell is shared by exactly 2 cells of the honeycomb.
;;; Keep last 4 visited honeycomb cells in buffer to avoid testing
;;; same cell twice and to insure they are visited in proper order.
;;; This probably is not the most efficient way, but it's good enough.
(defun rasterize-honeycomb (start end callback)
  (let ((start*2 (vec* start 2.0))
        (end*2 (vec* end 2.0))
        (buffer nil))
    (flet ((push-cell (c)
             (unless (find-if (lambda (a) (equal (car a) c)) buffer)
               (push (cons c (distance-squared start*2 (coerce-vec c)))
                     buffer)))
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

;;; In honeycomb hc, find closest cell and the face being hit
;;; by line segment start-end
(defun find-closest-hit (start end)
  (block stepper
    (with-slots (cell-values) *honeycomb*
      (rasterize-honeycomb start end
        (lambda (x y z)
          (let* ((pos (vec x y z))
                 (cell (world-to-lattice-int pos))
                 (i (aref cell 0))
                 (j (aref cell 1))
                 (k (aref cell 2)))
            (let (face)
              (when (and (array-in-bounds-p cell-values i j k)
                         (not (zerop (aref cell-values i j k)))
                         (setf face (line-troct-intersection start end pos)))
                (return-from stepper (values pos face))))))))))

(defun remove-cell (center)
  (let ((cell (world-to-lattice-int center))
        (cv (slot-value *honeycomb* 'cell-values)))
    (setf (aref cv (aref cell 0) (aref cell 1) (aref cell 2)) 0
          *scene-modified* t)))

(defun add-cell (center face value)
  (let ((cell (world-to-lattice-int center))
        (cv (slot-value *honeycomb* 'cell-values)))
    (multiple-value-bind (i j k) (neighbour-cell (aref cell 0)
                                                 (aref cell 1)
                                                 (aref cell 2)
                                                 face)
      (when (array-in-bounds-p cv i j k)
        (setf (aref cv i j k) value
              *scene-modified* t)))))

(defun cell-value (i j k)
  (with-slots (cell-values) *honeycomb*
    (if (array-in-bounds-p cell-values i j k)
      (aref cell-values i j k)
      0)))
