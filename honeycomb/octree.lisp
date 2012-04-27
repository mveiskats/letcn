(in-package :letcn)

(defparameter *disable-occlusion-culling* t)
(defparameter *disable-detailed-cells* t)

;;; This contains the state of honeycomb
(defparameter *honeycomb* nil)

(defparameter +hc-leaf-size+ 32)

;;; TODO: Probably would be a good idea to use
;;; Morton order (calling it "z-order" will probably be confusing)
;;; when referencing child nodes and grid cells
(defclass honeycomb (3d-object)
  ((cell-values :initarg :cell-values)
   (octree-root :initarg :octree-root)))

(defclass hc-node ()
  ((corner :initarg :corner)
   (height :initarg :height)
   (samples-visible :initform 0)
   (query-id :initform nil)
   (bounder-list-id :initform nil)))

(defclass hc-partition (hc-node)
  ((children :initform (make-array '(2 2 2) :initial-element nil))))

(defclass hc-leaf (hc-node)
  ((detailed-list-id :initform nil)
   (vert-buffer-id :initform nil)
   (col-buffer-id :initform nil)
   (buffer-size :initform nil)))

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
  (with-transformation +l2w-transform+
    (gl:with-pushed-matrix
      (gl:load-matrix (get-transformation))
      (use-current-program)
      (draw (slot-value hc 'octree-root)))))

(defmethod post-process ((hc honeycomb))
  (post-process (slot-value hc 'octree-root)))

(defun compile-bounder (node)
  (with-slots (height corner) node
    (let ((corners (make-array '(2 2 2)))
          (size (node-size height))
          (list-id (setf (slot-value node 'bounder-list-id)
                         (gl:gen-lists 1))))
      (flet ((flip-vector (v dir) (if (zerop dir) (vec* v -1.0) v))
             (min-or-max (a) (if (zerop a) 0 (1- size))))
        (doarray (i j k) corners
          (setf (aref corners i j k)
                (reduce #'vec+
                        (mapcar #'flip-vector +cell-bounds+ (list i j k))
                        :initial-value (coerce-vec (mapcar #'min-or-max
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
                (gl:vertex (+ (aref v 0) (aref corner 0))
                           (+ (aref v 1) (aref corner 1))
                           (+ (aref v 2) (aref corner 2)))))))))))

;;; Draw a bounding rhombohedron for honeycomb of given size
;;; offset from the specified grid cell
(defun draw-honeycomb-bounder (node)
  (with-slots (height) node
    (when (eq (slot-value node 'bounder-list-id) nil)
      (compile-bounder node))

    ;; Can't have our bounding boxes showing
    (gl:color-mask nil nil nil nil)
    (gl:depth-mask nil)
    (gl:disable :lighting)

    (gl:call-list (slot-value node 'bounder-list-id))

    ;; Put things back the way they were
    (gl:color-mask :true :true :true :true)
    (gl:depth-mask t)
    (gl:enable :lighting)))

;;; Draw given node if it has been visible last frame.
;;; If not, draw bounding box instead
;;; TODO: should look into making gl:gen-queries return
;;; multiple values just like gl:gen-lists does
(defmethod draw :around ((node hc-node))
  (with-slots (query-id corner samples-visible height) node
    (if (or *disable-occlusion-culling* (> samples-visible 0))
      (call-next-method)
      (progn
        (when (eq query-id nil) (setf query-id (car (gl:gen-queries 1))))
        (gl:begin-query :samples-passed query-id)
        (draw-honeycomb-bounder node)
        (gl:end-query :samples-passed)))))

(defmethod post-process :around ((node hc-node))
  (with-slots (query-id samples-visible) node
    (if (or *disable-occlusion-culling* (> samples-visible 0))
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
  (with-slots (vert-buffer-id col-buffer-id buffer-size corner) leaf
    (unless vert-buffer-id
      ;; Got no buffers - better generate them
      ;; vertex-attrib-pointer was behaving weirdly with non-zero stride,
      ;; so made two buffers for now
      (let* ((max-size (* 6 +hc-leaf-size+ +hc-leaf-size+ +hc-leaf-size+))
             (buffers (gl:gen-buffers 2))
             (imin (aref corner 0))
             (jmin (aref corner 1))
             (kmin (aref corner 2))
             (imax (+ imin +hc-leaf-size+ -1))
             (jmax (+ jmin +hc-leaf-size+ -1))
             (kmax (+ kmin +hc-leaf-size+ -1)))
        (setf buffer-size 0)
        (cffi:with-foreign-object (vert-buffer-data :float max-size)
          (cffi:with-foreign-object (col-buffer-data :float max-size)
            (flet ((push-buffers (v c)
                     (setf (cffi:mem-aref vert-buffer-data :float buffer-size)
                           (coerce v 'float)
                           (cffi:mem-aref col-buffer-data :float buffer-size)
                           (coerce c 'float))
                   (incf buffer-size)))
              (loop for i from imin to imax
                    do (loop for j from jmin to jmax
                             do (loop for k from kmin to kmax
                                      do (unless (zerop (cell-value i j k))
                                           (multiple-value-bind (r g b)
                                               (cell-color i j k)
                                             (push-buffers i r)
                                             (push-buffers j g)
                                             (push-buffers k b)))))))

            (setf vert-buffer-id (first buffers))
            (gl:bind-buffer :array-buffer vert-buffer-id)
            (%gl:buffer-data :array-buffer
                             (* buffer-size float-size)
                             vert-buffer-data
                             :static-draw)
            (setf col-buffer-id (second buffers))
            (gl:bind-buffer :array-buffer col-buffer-id)
            (%gl:buffer-data :array-buffer
                             (* buffer-size float-size)
                             col-buffer-data
                             :static-draw)))))

      (gl:enable-vertex-attrib-array 0)
      (gl:enable-vertex-attrib-array 1)

      (gl:bind-buffer :array-buffer vert-buffer-id)
      (%gl:vertex-attrib-pointer 0 3 :float :false 0 (cffi:make-pointer 0))
      (gl:bind-buffer :array-buffer col-buffer-id)
      (%gl:vertex-attrib-pointer 1 3 :float :false 0 (cffi:make-pointer 0))
      (gl:draw-arrays :points 0 (truncate buffer-size 3))
      (gl:disable-vertex-attrib-array 0)
      (gl:disable-vertex-attrib-array 1)))

(defmethod draw ((node hc-leaf))
  (with-slots (query-id) node
    ;; TODO: copypasta
    (when (eq query-id nil) (setf query-id (car (gl:gen-queries 1))))
    (gl:begin-query :samples-passed query-id)
    (if (or *disable-detailed-cells*
            (> (distance-squared (slot-value *camera* 'position)
                                 (node-position node))
               +lod-distance-squared+))
      (progn
        (gl:point-size 5) ;; TODO: point size
        (draw-simple node))
      (draw-detailed node))
    (gl:end-query :samples-passed)))

(defmethod post-process ((node hc-leaf))
  (with-slots (query-id samples-visible) node
    ;; Ideally we should check if query is done and
    ;; give something for cpu to do while it finishes
    ;; TODO: copypasta
    (setf samples-visible (get-query-object-uiv query-id :query-result))))

(defun make-honeycomb (size)
  (let ((result (make-array (list size size size)
                            :element-type 'integer
                            :initial-element 0))
        (octree-height (ceiling (log (/ size +hc-leaf-size+) 2))))
    (doarray (i j k) result
      (let ((p (lattice-to-world (coerce-vec (list i j k)))))
        (let ((n (* 10 (noise3d-octaves (/ (aref p 0) 10)
                                        (/ (aref p 1) 10)
                                        (/ (aref p 2) 10)
                                        2 0.25))))
          (if (> 0.3 n -0.3)
            (setf (aref result i j k) (cond ((> -0.2 n) 1)
                                            ((> 0.2 n) 2)
                                            (t 3)))))))

    (make-instance 'honeycomb
                   :cell-values result
                   :octree-root (make-hc-node #(0 0 0) octree-height))))
