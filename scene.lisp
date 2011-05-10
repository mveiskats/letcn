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

  (make-honeycomb 32))

(defun draw-scene (scene camera)
  (with-slots (position rotation) camera
    (gl:load-identity)
    (gl:mult-matrix rotation)
    (gl:translate (- (aref position 0))
                  (- (aref position 1))
                  (- (aref position 2)))
    
    ;; (setf scene
    ;;       (flet ((cam-dist (s) (distance position (slot-value s 'position))))
    ;;         (sort (copy-list scene) (lambda (s1 s2)
    ;;                                   (> (cam-dist s1) (cam-dist s2))))))

    ;; (dolist (obj scene)
    ;;   (draw obj))

    (when *scene-modified*
      (gl:with-new-list (1 :compile)
        (gl:enable :cull-face)
        (gl:front-face :ccw)
        (gl:cull-face :back)
        (gl:color 0.3 0.7 0.3)
        (draw scene))
      (setf *scene-modified* nil))

    (gl:enable :polygon-offset-fill)
    (gl:polygon-offset 1.0 1.0)
    (gl:call-list 1)
    (gl:disable :polygon-offset-fill)

    (multiple-value-bind (center face)
        (find-closest-hit position
                          (vector+ position (matrix*vector rotation
                                                           #(0.0 0.0 -5.0 0.0)))
                          scene)
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
