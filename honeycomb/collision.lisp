(in-package :letcn)

;; Calls a function for every cell of cubic honeycomb
;; the given line segment passes through.
;; Cell width is 1 and they are identified by the corner
;; with lowest coordinates.
(defun rasterize-segment (x0 y0 z0 x1 y1 z1 callback)
  (let* ((dx (- x1 x0))
         (dy (- y1 y0))
         (dz (- z1 z0))
         (stepx (if (> x1 x0) 1 -1))
         (stepy (if (> y1 y0) 1 -1))
         (stepz (if (> z1 z0) 1 -1))
         (x (if (> dx 0.0) (floor x0) (ceiling x0)))
         (y (if (> dy 0.0) (floor y0) (ceiling y0)))
         (z (if (> dz 0.0) (floor z0) (ceiling z0)))
         ;; Usually in parametric equations this is denoted by t,
         ;; but this is a bad idea in common lisp
         (px (unless (= dx 0.0) (/ (- (+ x stepx) x0) dx)))
         (py (unless (= dy 0.0) (/ (- (+ y stepy) y0) dy)))
         (pz (unless (= dz 0.0) (/ (- (+ z stepz) z0) dz))))
    (flet ((minp ()
             ;; Find minimum of the three while discarding nil values
             (if (and px (or (not py) (< px py)) (or (not pz) (< px pz))) px
               (if (and py (or (not pz) (< py pz))) py pz)))
           (output-step ()
             (funcall callback
                      (if (> dx 0.0) x (1- x))
                      (if (> dy 0.0) y (1- y))
                      (if (> dz 0.0) z (1- z)))))
      (loop with p
            do (output-step)
            while (<= (setf p (minp)) 1.0)
            do (progn
                 (when (and px (<= px p))
                   (incf x stepx)
                   (setf px (/ (- (+ x stepx) x0) dx)))
                 (when (and py (<= py p))
                   (incf y stepy)
                   (setf py (/ (- (+ y stepy) y0) dy)))
                 (when (and pz (<= pz p))
                   (incf z stepz)
                   (setf pz (/ (- (+ z stepz) z0) dz))))))))

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
      (rasterize-segment (aref start*2 0) (aref start*2 1) (aref start*2 2)
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
