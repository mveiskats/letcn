(in-package :letcn)

(defparameter *camera* nil)

(defclass camera (3d-object)
  ())

(defun make-scene ()
  (setf *honeycomb* (make-honeycomb 64)))

;; TODO: make a patch for cl-opengl
(defun get-query-object-uiv (id pname)
  (cffi:with-foreign-object (result '%gl:uint)
    (%gl::get-query-object-uiv id pname result)
    (cffi:mem-ref result '%gl:uint)))

(defun world-to-local-matrix (obj)
  (with-slots (position rotation) obj
    (matrix* (quat-to-matrix rotation)
             (translate (vec* position -1.0)))))

(defun draw-scene ()
  (with-transformation (world-to-local-matrix *camera*)
    (gl:enable :polygon-offset-fill)
    (gl:polygon-offset 1.0 1.0)
    (draw *honeycomb*)
    (post-process *honeycomb*)
    (with-slots (position rotation) *camera*
      (draw-highlight position rotation))
    (gl:disable :polygon-offset-fill)))

(defun draw-highlight(position rotation)
  (multiple-value-bind (center face)
      (let ((front (vec+ position
                         (quat-rotate (vec 0.0 0.0 -5.0)
                                      (quat-inverse rotation)))))
        (find-closest-hit position front))
    (cond (center
            (setf *highlight* (cons center face))
            (gl:polygon-offset 0.0 1.0)

            (gl:use-program *program*)
            (set-shader-vars *program*)
            (gl:with-primitives :points
              (gl:vertex-attrib 1 1.0 1.0 1.0)
              (gl:vertex (aref center 0) (aref center 1) (aref center 2)))
            (gl:use-program 0))
          (t (setf *highlight* nil)))))

(defun rotate-camera (dx dy)
  (with-slots (rotation) *camera*
    (let ((rot-x (axis-angle-to-quat +up+ dx))
          (rot-y (axis-angle-to-quat +right+ dy)))
      (setf rotation (normalize-quat (quat* (quat* rot-x rot-y) rotation))))))

(defun move-camera (direction)
  (with-slots (position rotation) *camera*
    (let* ((world-dir (quat-rotate direction (quat-inverse rotation)))
           (new-pos (vec+ world-dir position)))
      (multiple-value-bind (p n)
          (sphere-honeycomb-intersection position new-pos 1.5)
        (when p
          (let* ((pushback-len (* (/ (distance new-pos p)
                                     (vec-length world-dir))
                                  (dot-product n (vec* world-dir -1.0))))
                 (pushback-pos (vec+ new-pos (vec* n (+ pushback-len 0.01)))))
            ;; TODO: check collision for the slide
            (setf new-pos pushback-pos)))
        (setf position new-pos)))))

(defun pass-time (dt)
  (with-slots (position velocity) *camera*
    ;; TODO: proper gravity calculations
    (let ((gravity (normalize (vec- (slot-value *honeycomb* 'center-of-mass)
                                    position))))
      (setf velocity (vec+ velocity (vec* gravity dt))))
    (let* ((new-pos (vec+ position (vec* velocity dt)))
           (p (sphere-honeycomb-intersection position new-pos 1.5)))
       (when p
         ;; pushback
         (setf new-pos (vec+ p (vec* (normalize velocity) -0.01))
               velocity (vec 0.0 0.0 0.0)))
       (setf position new-pos))))
