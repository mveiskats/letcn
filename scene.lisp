(in-package :letcn)

(defclass camera ()
  ((position :initform #(0.0 0.0 0.0) :initarg :position)
   (rotation :initform identity-matrix :initarg :rotation)))

(defun make-scene ()
  ;; (let ((hv (hyperboloid-vertices 1 1))
  ;;       (scene nil))
  ;;   (dotimes (i (array-dimension hv 0))
  ;;     (let ((s (make-fuzzy-sphere 0.3 500)))
  ;;       (setf (slot-value s 'position)
  ;;             (list (aref hv i 0)
  ;;                   (aref hv i 1)
  ;;                   (aref hv i 2)))
  ;;       (push s scene)))
  ;;   scene)

  (setf *honeycomb* (make-honeycomb 32))
  (setf *hc-octree* (make-hc-node #(0 0 0) 3)))

;; TODO: make a patch for cl-opengl
(defun get-query-object-uiv (id pname)
  (cffi:with-foreign-object (result '%gl:uint)
    (%gl::get-query-object-uiv id pname result)
    (cffi:mem-ref result '%gl:uint)))

(defun draw-scene (scene camera)
  (with-slots (position rotation) camera
    (gl:load-identity)
    (gl:mult-matrix rotation)
    (gl:translate (- (aref position 0))
                  (- (aref position 1))
                  (- (aref position 2)))
    
    (draw *hc-octree*)
    (post-process *hc-octree*)

    (multiple-value-bind (center face)
      (find-closest-hit position
                        (vector+ position
                                 (matrix*vector rotation #(0.0 0.0 -5.0 0.0))))
      (if (eq center nil)
        (setf *highlight* nil)
        (progn
          (setf *highlight* (cons center face))
          (draw-highlight center face))))))

(defun rotate-camera (camera dx dy)
  (with-slots (rotation) camera
    (let ((rot-x (rotation-matrix #(0.0 1.0 0.0) dx))
          (rot-y (rotation-matrix #(1.0 0.0 0.0) dy)))
      (setf rotation (matrix*matrix (matrix*matrix rotation rot-x) rot-y)))))
