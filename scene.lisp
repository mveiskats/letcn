(in-package :letcn)

(defparameter *camera* nil)

(defclass camera ()
  ((position :initform #(0.0 0.0 0.0) :initarg :position)
   (rotation :initform identity-matrix :initarg :rotation)))

(defun make-scene ()
  (setf *honeycomb* (make-honeycomb 32))
  (setf *hc-octree* (make-hc-node #(0 0 0) 3)))

;; TODO: make a patch for cl-opengl
(defun get-query-object-uiv (id pname)
  (cffi:with-foreign-object (result '%gl:uint)
    (%gl::get-query-object-uiv id pname result)
    (cffi:mem-ref result '%gl:uint)))

(defun draw-scene ()
  (with-slots (position rotation) *camera*
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

(defun rotate-camera (dx dy)
  (with-slots (rotation) *camera*
    (let ((rot-x (rotation-matrix #(0.0 1.0 0.0) dx))
          (rot-y (rotation-matrix #(1.0 0.0 0.0) dy)))
      (setf rotation (matrix*matrix (matrix*matrix rotation rot-x) rot-y)))))

(defun move-camera (direction)
  (with-slots (position rotation) *camera*
    (setf position (map 'vector #'+
                        (matrix*vector rotation direction)
                        position))))
