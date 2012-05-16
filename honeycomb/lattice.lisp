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
