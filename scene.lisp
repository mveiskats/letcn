(in-package :letcn)

(defparameter *camera* nil)

(defclass camera ()
  ((position :initform (vec 0.0 0.0 0.0) :initarg :position)
   (rotation :initform +identity-matrix+ :initarg :rotation)))

(defun make-scene ()
  (setf *honeycomb* (make-honeycomb 64)))

;; TODO: make a patch for cl-opengl
(defun get-query-object-uiv (id pname)
  (cffi:with-foreign-object (result '%gl:uint)
    (%gl::get-query-object-uiv id pname result)
    (cffi:mem-ref result '%gl:uint)))

(defun draw-scene ()
  (with-slots (position rotation) *camera*
    (with-transformation (matrix* rotation (translate (vec* position -1.0)))
      (draw *honeycomb*)
      (post-process *honeycomb*)
      ;(draw-highlight)
      )))

(defun draw-highlight()
  (multiple-value-bind (center face)
      (find-closest-hit position
                        (vec+ position
                              (transform-direction (vec 0.0 0.0 -5.0)
                                                   rotation)))
    (if (eq center nil)
        (setf *highlight* nil)
      (progn
        (setf *highlight* (cons center face))
        (draw-highlight center face)))))

(defun rotate-camera (dx dy)
  (with-slots (rotation) *camera*
    (let ((rot-x (rotate-around (vec 0.0 1.0 0.0) dx))
          (rot-y (rotate-around (vec 1.0 0.0 0.0) dy)))
      (setf rotation (matrix* rot-x rot-y rotation)))))

(defun move-camera (direction)
  (with-slots (position rotation) *camera*
    (setf position (vec+ (transform-direction direction
                                              (inverse-matrix rotation))
                         position))))
