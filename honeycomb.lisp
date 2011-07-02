(in-package :letcn)

;;; These contain the state of honeycomb
(defparameter *honeycomb* nil)

(defconstant +hc-leaf-size+ 4)

;;; Helper methods to translate between world position
;;; and grid coordinates of cell
(let* ((g2w #2A((0.5 0.0 0.5)
                (0.5 1.0 0.5)
                (0.5 0.0 -0.5)))
       (w2g (invert-matrix g2w)))
  (defun grid-to-world (g) (matrix*vector g2w g))
  (defun world-to-grid (w) (matrix*vector w2g w))
  (defun world-to-grid-int (w) (map 'vector #'truncate (world-to-grid w))))

;;; Midpoint of each face scaled by 2 is center of a neighbouring cell
(defvar +cell-neighbours+
  (flet ((vertex-sum (face)
           (apply #'mapcar #'+
                  (mapcar (lambda (v) (aref +troct-vertices+ v)) face))))
    (map 'vector
         (lambda (f) (world-to-grid-int (vector* (vertex-sum f)
                                                 (/ 2 (length f)))))
         +troct-faces+)))

;;; Determines indices of a neighbour cell
(defun neighbour-cell (i j k face)
  (let ((neighbour (aref +cell-neighbours+ face)))
    (values (+ i (aref neighbour 0))
            (+ j (aref neighbour 1))
            (+ k (aref neighbour 2)))))

;;; 3 vectors from which bounding rhombohedron will be generated
;;; Finds them by going through all vertices and determining which one
;;; is furthest away from plane [axis-a, axis-b] along axis-c
;;; TODO: Do this by converting vertices to basis of { axis-a, axis-b, axis-c }
(defvar +cell-bounds+
  (let* ((axis-i (grid-to-world #(1 0 0)))
         (axis-j (grid-to-world #(0 1 0)))
         (axis-k (grid-to-world #(0 0 1))))
    (labels ((max-scale (v1 v2 v3)
               (loop for vert across +troct-vertices+
                     maximize (line-plane-intersection vert (vector- vert v1)
                                                       #(0 0 0) v2 v3)))
             (get-bounder (v1 v2 v3) (vector* v1 (max-scale v1 v2 v3))))
      (list (get-bounder axis-i axis-j axis-k)
            (get-bounder axis-j axis-i axis-k)
            (get-bounder axis-k axis-i axis-j)))))

(defun compile-bounder (list-id node-height)
  (let ((corners (make-array '(2 2 2)))
        (size (node-size node-height)))
    (flet ((flip-vector (v dir) (if (zerop dir) (vector* v -1) v))
           (min-or-max (a) (if (zerop a) 0 (1- size))))
      (doarray (i j k) corners
        (setf (aref corners i j k)
              (reduce #'vector+
                      (mapcar #'flip-vector +cell-bounds+ (list i j k))
                      :initial-value (grid-to-world (map 'vector
                                                         #'min-or-max
                                                         (list i j k)))))))

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
  (let ((world-offset (grid-to-world grid-offset)))
    (with-slots (bounders) *honeycomb*
      (when (eq (aref bounders node-height) nil)
        (compile-bounder (setf (aref bounders node-height)
                               (gl:gen-lists 1))
                         node-height))

      (gl:with-pushed-matrix
        (gl:translate (aref world-offset 0)
                      (aref world-offset 1)
                      (aref world-offset 2))
        ;; Can't have our bounding boxes showing
        (gl:color-mask nil nil nil nil)
        (gl:depth-mask nil)
        (gl:disable :lighting)

        (gl:call-list (aref bounders node-height))

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
      (let ((p (grid-to-world (make-vector i j k))))
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

;;; In honeycomb hc, find closest cell and the face being hit
;;; by line segment start-end
(defun find-closest-hit (start end)
  (block stepper
    (with-slots (cell-values) *honeycomb*
      (rasterize-honeycomb start end
        (lambda (x y z)
          (let* ((pos (make-vector x y z))
                 (cell (world-to-grid-int pos))
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
  (gl:with-pushed-matrix
    (gl:translate (aref center 0) (aref center 1) (aref center 2))
    (draw-troct-face idx)))

(defun remove-cell (center)
  (let ((cell (world-to-grid-int center))
        (cv (slot-value *honeycomb* 'cell-values)))
    (setf (aref cv (aref cell 0) (aref cell 1) (aref cell 2)) 0
          *scene-modified* t)))

(defun add-cell (center face value)
  (let ((cell (world-to-grid-int center))
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

(defun draw-cell (i j k)
  (case (cell-value i j k)
    (1 (gl:color 0.7 0.3 0.3))
    (2 (gl:color 0.3 0.7 0.3))
    (t (gl:color 0.3 0.3 0.7)))
  (let ((center (grid-to-world (make-vector i j k))))
    (gl:with-pushed-matrix
      (gl:translate (aref center 0) (aref center 1) (aref center 2))
      (dotimes (idx (length +troct-faces+))
        (multiple-value-bind (ii jj kk) (neighbour-cell i j k idx)
          (when (zerop (cell-value ii jj kk))
            (draw-troct-face idx)))))))

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
  ((list-id :initform nil)))

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
              (make-hc-node (vector+ grid-offset
                                     (vector* (make-vector i j k) half-size))
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

;;; Draw child nodes in z-order
(defmethod draw ((node hc-partition))
  (with-slots (children height) node
    (let* ((half-size (node-size (1- height)))
           (distance (vector- (slot-value *camera* 'position)
                              (vector+ (slot-value *honeycomb* 'position)
                                       (grid-to-world (make-vector half-size
                                                                   half-size
                                                                   half-size)))))
           (order (sort (make-vector 0 1 2) #'> :key (lambda (a) (abs (aref distance a)))))
           (backwards (map 'vector (lambda (a) (< 0 a)) distance)))
      (doarray (i j k) children
        (let ((coords (make-vector 0 0 0)))
          ;; TODO: Verify if this is right
          (setf (aref coords (aref order 0)) (if (aref backwards 0) (- 1 i) i)
                (aref coords (aref order 1)) (if (aref backwards 1) (- 1 j) j)
                (aref coords (aref order 2)) (if (aref backwards 2) (- 1 k) k))
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

(defmethod draw ((node hc-leaf))
  (with-slots (query-id list-id corner) node
    (when (eq list-id nil)
      ;; Got no display list - better generate one
      (setf list-id (gl:gen-lists 1))
      (gl:with-new-list (list-id :compile)
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
    ;; TODO: copypasta
    (when (eq query-id nil) (setf query-id (car (gl:gen-queries 1))))
    (gl:begin-query :samples-passed query-id)
    (gl:call-list list-id)
    (gl:end-query :samples-passed)))

(defmethod post-process ((node hc-leaf))
  (with-slots (query-id samples-visible) node
    ;; Ideally we should check if query is done and
    ;; give something for cpu to do while it finishes
    ;; TODO: copypasta
    (setf samples-visible (get-query-object-uiv query-id :query-result))))
