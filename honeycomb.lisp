(in-package :letcn)

;;; These contain the state of honeycomb
(defparameter *honeycomb* nil)

(defconstant +hc-leaf-size+ 4)

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

;;; Midpoint of each face scaled by 2 is center of a neighbouring cell
(defvar +cell-neighbours+
  (labels ((vertex-sum (face)
             (reduce #'vec+
                    (mapcar (lambda (v) (aref +troct-vertices+ v)) face)
                    :initial-value (vec 0.0 0.0 0.0)))
           (neighbour-grid-offset (face)
             (world-to-lattice-int (vec* (vertex-sum face)
                                      (/ 2.0 (length face))))))
    (map 'vector #'neighbour-grid-offset +troct-faces+)))

;;; Determines indices of a neighbour cell
(defun neighbour-cell (i j k face)
  (let ((neighbour (aref +cell-neighbours+ face)))
    (values (+ i (aref neighbour 0))
            (+ j (aref neighbour 1))
            (+ k (aref neighbour 2)))))

;;; 3 vectors from which bounding rhombohedron will be generated
(defvar +cell-bounds+
  (loop with grid-vert
        for vert across +troct-vertices+
        do (setf grid-vert (world-to-lattice vert))
        maximize (aref grid-vert 0) into max-x
        maximize (aref grid-vert 1) into max-y
        maximize (aref grid-vert 2) into max-z
        finally (return (list (lattice-to-world (vec max-x 0.0 0.0))
                              (lattice-to-world (vec 0.0 max-y 0.0))
                              (lattice-to-world (vec 0.0 0.0 max-z))))))

(defun compile-bounder (list-id node-height)
  (let ((corners (make-array '(2 2 2)))
        (size (node-size node-height)))
    (flet ((flip-vector (v dir) (if (zerop dir) (vec* v -1.0) v))
           (min-or-max (a) (if (zerop a) 0 (1- size))))
      (doarray (i j k) corners
        (setf (aref corners i j k)
              (reduce #'vec+
                      (mapcar #'flip-vector +cell-bounds+ (list i j k))
                      :initial-value (lattice-to-world (coerce-vec (mapcar #'min-or-max
                                                                        (list i j k))))))))

    ;; These turn out visible even if it seems
    ;; the visible side should be cw ... aaaaargh
    (gl:with-new-list (list-id :compile)
      (dolist (face '(((0 0 0) (1 0 0) (1 1 0) (0 1 0)) ;xy
                      ((0 0 1) (0 1 1) (1 1 1) (1 0 1))
                      ((0 0 0) (0 0 1) (1 0 1) (1 0 0)) ;xz
                      ((0 1 0) (1 1 0) (1 1 1) (0 1 1))
                      ((0 0 0) (0 1 0) (0 1 1) (0 0 1)) ;yz
                      ((1 0 0) (1 0 1) (1 1 1) (1 1 0))))
        (gl:with-primitives :polygon
          (dolist (idx face)
            (let ((v (aref corners (first idx) (second idx) (third idx))))
              (gl:vertex (aref v 0) (aref v 1) (aref v 2)))))))))

;;; Draw a bounding rhombohedron for honeycomb of given size
;;; offset from the specified grid cell
(defun draw-honeycomb-bounder (grid-offset node-height)
  (let ((world-offset (lattice-to-world (coerce-vec grid-offset))))
    (with-slots (bounders) *honeycomb*
      (when (eq (aref bounders node-height) nil)
        (compile-bounder (setf (aref bounders node-height)
                               (gl:gen-lists 1))
                         node-height))

      (with-transformation (translate world-offset)
        ;; Can't have our bounding boxes showing
        (gl:color-mask nil nil nil nil)
        (gl:depth-mask nil)
        (gl:disable :lighting)

        (gl:with-pushed-matrix
          (gl:load-matrix *transformation*)
          (use-current-program)
          (gl:call-list (aref bounders node-height)))

        ;; Put things back the way they were
        (gl:color-mask :true :true :true :true)
        (gl:depth-mask t)
        (gl:enable :lighting)))))

(defun make-honeycomb (size)
  (let ((result (make-array (list size size size)
                            :element-type 'integer
                            :initial-element 0))
        (octree-height (ceiling (log (/ size +hc-leaf-size+) 2))))
    (doarray (i j k) result
      (let ((p (lattice-to-world (coerce-vec (list i j k)))))
        (if (> 0 (* 10 (noise3d-octaves (/ (aref p 0) 10)
                                        (/ (aref p 1) 10)
                                        (/ (aref p 2) 10)
                                        3 0.25)))
            (setf (aref result i j k) 1))))

    (make-instance 'honeycomb
                   :cell-values result
                   :octree-root (make-hc-node #(0 0 0) octree-height)
                   :bounders (make-array (1+ octree-height)
                                         :initial-element nil))))

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

(defun draw-highlight (center idx)
  (gl:color 0.5 0.0 0.0)
    (draw-troct-face idx center))

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

(defun emit-cell-color (i j k)
  (case (cell-value i j k)
    (1 (gl:color 0.7 0.3 0.3))
    (2 (gl:color 0.3 0.7 0.3))
    (t (gl:color 0.3 0.3 0.7))))

(defun draw-cell (i j k)
  (emit-cell-color i j k)
  (let ((center (lattice-to-world (coerce-vec (list i j k)))))
    (dotimes (idx (length +troct-faces+))
      (multiple-value-bind (ii jj kk) (neighbour-cell i j k idx)
        (when (zerop (cell-value ii jj kk))
          (draw-troct-face idx center))))))

;;; TODO: Probably would be a good idea to use
;;; Morton order (calling it "z-order" will probably be confusing)
;;; when referencing child nodes and grid cells
(defclass honeycomb (3d-object)
  ((cell-values :initarg :cell-values)
   (octree-root :initarg :octree-root)
   (bounders :initarg :bounders)))

(defclass hc-node ()
  ((corner :initarg :corner)
   (height :initarg :height)
   (samples-visible :initform 0)
   (query-id :initform nil)))

(defclass hc-partition (hc-node)
  ((children :initform (make-array '(2 2 2) :initial-element nil))))

(defclass hc-leaf (hc-node)
  ((detailed-list-id :initform nil)
   (simple-list-id :initform nil)))

(defun node-size (height)
  (* +hc-leaf-size+ (expt 2 height)))

(defun make-hc-node (grid-offset tree-height)
  (if (= tree-height 0)
    (make-hc-leaf grid-offset)
    (make-hc-partition grid-offset tree-height)))

(defun make-hc-leaf (grid-offset)
  (make-instance 'hc-leaf
                 :corner grid-offset
                 :height 0))

(defun make-hc-partition (grid-offset tree-height)
  (let* ((half-size (node-size (1- tree-height)))
         (result (make-instance 'hc-partition
                                :corner grid-offset
                                :height tree-height)))
    (with-slots (children) result
      (doarray (i j k) children
        (setf (aref children i j k)
              (make-hc-node (map 'vector #'+
                                 grid-offset
                                 (list (* i half-size)
                                       (* j half-size)
                                       (* k half-size)))
                            (1- tree-height)))))
    result))

;;; At the moment just polls the results for visibility queries
(defgeneric post-process (obj))

(defmethod draw ((hc honeycomb))
  (draw (slot-value hc 'octree-root)))

(defmethod post-process ((hc honeycomb))
  (post-process (slot-value hc 'octree-root)))

;;; Draw given node if it has been visible last frame.
;;; If not, draw bounding box instead
;;; TODO: should look into making gl:gen-queries return
;;; multiple values just like gl:gen-lists does
(defmethod draw :around ((node hc-node))
  (with-slots (query-id corner samples-visible height) node
    (if (> samples-visible 0)
      (call-next-method)
      (progn
        (when (eq query-id nil) (setf query-id (car (gl:gen-queries 1))))
        (gl:begin-query :samples-passed query-id)
        (draw-honeycomb-bounder corner height)
        (gl:end-query :samples-passed)))))

(defmethod post-process :around ((node hc-node))
  (with-slots (query-id samples-visible) node
    (if (> samples-visible 0)
      (call-next-method)
      ;; Ideally we should check if query is done and
      ;; give something for cpu to do while it finishes
      (setf samples-visible (get-query-object-uiv query-id :query-result)))))

;;; FIXME: doesnt take in account honeycomb rotation
(defun node-position (node)
  (with-slots (corner height) node
  (let* ((half-size (node-size (1- height)))
         (node-midpoint (coerce-vec (list (+ (aref corner 0) half-size)
                                          (+ (aref corner 1) half-size)
                                          (+ (aref corner 2) half-size)))))
    (vec+ (slot-value *honeycomb* 'position)
          (lattice-to-world node-midpoint)))))

;;; Draw child nodes in z-order
(defmethod draw ((node hc-partition))
  (with-slots (children) node
    (let* ((distance (vec- (slot-value *camera* 'position)
                           (node-position node)))
           (order (sort (list 0 1 2) #'>
                        :key (lambda (a) (abs (aref distance a)))))
           (backwards (map 'vector (lambda (a) (< 0 a)) distance)))
      (doarray (i j k) children
        (let ((coords (make-array '(3) :initial-element 0)))
          ;; TODO: Verify if this is right
          (setf (aref coords (car order)) (if (aref backwards 0) (- 1 i) i)
                (aref coords (cadr order)) (if (aref backwards 1) (- 1 j) j)
                (aref coords (caddr order)) (if (aref backwards 2) (- 1 k) k))
          (draw (aref children
                      (aref coords 0)
                      (aref coords 1)
                      (aref coords 2))))))))

(defmethod post-process ((node hc-partition))
  (with-slots (children samples-visible) node
    (let ((child-samples-visible 0))
      (doarray (i j k) children
        (post-process (aref children i j k))
        (incf child-samples-visible
              (slot-value (aref children i j k) 'samples-visible)))
      ;; Update nodes samples-visible to sum of all childrens samples-visible
      ;; so it will draw bounder next frame instead of individual children
      (setf samples-visible child-samples-visible))))

(defun draw-detailed (leaf)
  (with-slots (detailed-list-id corner) leaf
    (when (eq detailed-list-id nil)
      ;; Got no display list - better generate one
      (setf detailed-list-id (gl:gen-lists 1))
      (gl:with-new-list (detailed-list-id :compile)
        (let* ((imin (aref corner 0))
               (jmin (aref corner 1))
               (kmin (aref corner 2))
               (imax (+ imin +hc-leaf-size+ -1))
               (jmax (+ jmin +hc-leaf-size+ -1))
               (kmax (+ kmin +hc-leaf-size+ -1)))
          (loop for i from imin to imax
                do (loop for j from jmin to jmax
                         do (loop for k from kmin to kmax
                                  do (unless (zerop (cell-value i j k))
                                       (draw-cell i j k))))))))
    (gl:call-list detailed-list-id)))

(defun draw-simple (leaf)
  (with-slots (simple-list-id corner) leaf
    (when (eq simple-list-id nil)
      ;; Got no display list - better generate one
      (setf simple-list-id (gl:gen-lists 1))
      (gl:with-new-list (simple-list-id :compile)
        (let* ((imin (aref corner 0))
               (jmin (aref corner 1))
               (kmin (aref corner 2))
               (imax (+ imin +hc-leaf-size+ -1))
               (jmax (+ jmin +hc-leaf-size+ -1))
               (kmax (+ kmin +hc-leaf-size+ -1)))
          (gl:with-primitives :points
            (gl:normal 0.0 0.0 1.0) ;; This should point towards camera
            (loop for i from imin to imax
                  do (loop for j from jmin to jmax
                           do (loop for k from kmin to kmax
                                    do (unless (zerop (cell-value i j k))
                                         (emit-cell-color i j k)
                                         (let ((center (lattice-to-world (coerce-vec (list i j k)))))
                                         (gl:vertex (aref center 0)
                                                    (aref center 1)
                                                    (aref center 2)))))))))))
    (gl:call-list simple-list-id)))

(defmethod draw ((node hc-leaf))
  (with-slots (query-id) node
    ;; TODO: copypasta
    (when (eq query-id nil) (setf query-id (car (gl:gen-queries 1))))
    (gl:begin-query :samples-passed query-id)
    (gl:with-pushed-matrix
      (gl:load-matrix *transformation*)
      (use-current-program)
      (if (> (distance-squared (slot-value *camera* 'position)
                               (node-position node))
             +lod-distance-squared+)
        (progn
          (gl:point-size 1) ;; TODO: point size
          (draw-simple node))
        (draw-detailed node)))
    (gl:end-query :samples-passed)))

(defmethod post-process ((node hc-leaf))
  (with-slots (query-id samples-visible) node
    ;; Ideally we should check if query is done and
    ;; give something for cpu to do while it finishes
    ;; TODO: copypasta
    (setf samples-visible (get-query-object-uiv query-id :query-result))))
