(in-package :letcn)

(defparameter *blend* nil)
(defparameter *mouselook* nil)
(defparameter *left-mouse-down* nil)
(defparameter *right-mouse-down* nil)

(defparameter *forward-pressed* nil)
(defparameter *back-pressed* nil)
(defparameter *left-pressed* nil)
(defparameter *right-pressed* nil)

(defparameter *old-t* nil)
(defparameter *delta-t* nil)

(defparameter *highlight* nil)
(defparameter *scene-modified* t)
(defparameter *selected-tool* 1)

(defparameter *projection* nil)
(defparameter *program* nil)

(defparameter *shader-enabled* nil)

(defvar *mouse-sensitivity* 0.0007) ;; Pixel to radian ratio
(defvar *move-speed* 3)

(defclass letcn-window (glut:window)
  ()
  (:default-initargs
   :title "OMG, work already!"
   :mode '(:double :rgb)
   :width 800
   :height 600))

(defun make-shader (source type)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (let ((cffi:*default-foreign-encoding* :iso-8859-1)
          (log (gl:get-shader-info-log shader)))
      (unless (gl:get-shader shader :compile-status)
        (error "Failed to compile shader ~a~%~a~%" type log))
      (when (and log (> (length log) 0))
        (format t "shader-info-log for ~a~%~a~%" type log)))
    shader))

(defun make-vertex-shader ()
  (make-shader (vertex-shader-source) :vertex-shader))

(defun make-geometry-shader ()
  (make-shader (geometry-shader-source) :geometry-shader))

(defun make-fragment-shader ()
  (make-shader (fragment-shader-source) :fragment-shader))

(defun set-uniform-matrix (location matrix)
  (gl:uniform-matrix location 4 (make-array 1 :initial-element matrix) nil))

(defun use-current-program ()
  (if *shader-enabled*
    (progn
      (gl:use-program *program*)
      (set-uniform-matrix (gl:get-uniform-location *program* "model_transform")
                          (get-transformation))
      (set-uniform-matrix (gl:get-uniform-location *program* "view_transform")
                          *projection*)
      (gl:uniformfv (gl:get-uniform-location *program* "global_light_direction")
                    (transform-direction (vec 0.0 0.0 -1.0)
                                         (get-transformation))))
    (gl:use-program 0)))

(defmethod glut:display-window :before ((window letcn-window))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (make-scene)
  (setf *camera* (make-instance 'camera :position (vec 0.0 0.0 20.0)))

  (setf *program* (gl:create-program))
  (gl:attach-shader *program* (make-vertex-shader))
  (gl:attach-shader *program* (make-geometry-shader))
  (gl:attach-shader *program* (make-fragment-shader))

  (gl:link-program *program*)
  (gl:validate-program *program*)

  (let ((cffi:*default-foreign-encoding* :iso-8859-1)
        (log (gl:get-program-info-log *program*)))
    (unless (gl:get-program *program* :validate-status)
      (error "Shader program not valid~%~a~%" log))
    (when (and log (> (length log) 0))
      (format t "get-program-info-log~%~a~%" log))))

(defun toggle-blend ()
  (if *blend*
    (progn
      (setf *blend* nil)
      (gl:enable :depth-test)
      (gl:disable :blend))
    (progn
      (setf *blend* t)
      (gl:disable :depth-test)
      (gl:enable :blend))))

;;; Prints fps to console every 10 seconds
(let ((report-interval (* internal-time-units-per-second 10))
      (frames 0)
      (last-t nil))
  (defun count-fps ()
    (incf frames)
    (let ((current-t (get-internal-real-time)))
      (unless last-t (setf last-t current-t))
      (when (> (- current-t last-t) report-interval)
        (format t "~,2F fps~%" (/ frames 10))
        (setf last-t current-t
              frames 0)))))

;;; Cursor setting crashes sbcl on the second run.
(defun toggle-mouselook (window)
  (if *mouselook*
    (progn) ;;(glut:set-cursor :cursor-crosshair)
    (progn
      ;;(glut:set-cursor :cursor-none)
      (glut:warp-pointer (round (/ (glut:width window) 2))
                         (round (/ (glut:height window) 2)))))
  (setf *mouselook* (not *mouselook*)))

(defmethod glut:display ((window letcn-window))
  (block display
    (restart-case
     (progn
       (let ((current-t (get-internal-real-time)))
         (unless *old-t* (setf *old-t* current-t))
         (setf *delta-t* (- current-t *old-t*)
               *old-t* current-t))

       (gl:clear :color-buffer :depth-buffer)
       (gl:light :light0 :position #(-10.0 10.0 10.0 1.0))
       (gl:enable :lighting :light0 :depth-test :color-material)
       (gl:enable :cull-face)
       (gl:front-face :ccw)
       (gl:blend-func :src-alpha :one)

       (draw-scene)
       (let (move-directions)
         (when *forward-pressed* (push (vec 0.0 0.0 -1.0) move-directions))
         (when *back-pressed* (push (vec 0.0 0.0 1.0) move-directions))
         (when *left-pressed* (push (vec -1.0 0.0 0.0) move-directions))
         (when *right-pressed* (push (vec 1.0 0.0 0.0) move-directions))
         (when move-directions
           (move-camera (vec* (reduce #'vec+ move-directions
                                      :initial-value (vec 0.0 0.0 0.0))
                              (coerce (* *move-speed*
                                         (/ *delta-t*
                                            internal-time-units-per-second))
                                      'single-float)))))

       (glut:swap-buffers)
       (glut:post-redisplay)
       (count-fps))
     (quit-gracefully ()
       (progn (glut:destroy-current-window)
              (return-from display))))))

(defmethod glut:mouse ((window letcn-window) button state x y)
  (case button
    (:left-button (setf *left-mouse-down* (eq state :down))
                  (when (and (eq state :down)
                             (not (eq *highlight* nil)))
                    (remove-cell (car *highlight*))))
    (:right-button (setf *right-mouse-down* (eq state :down))
                   (when (and (eq state :down)
                              (not (eq *highlight* nil)))
                     (add-cell (car *highlight*)
                               (cdr *highlight*)
                               *selected-tool*)))))

(defmethod glut:passive-motion ((window letcn-window) x y)
  (let ((mid-x (round (/ (glut:width window) 2)))
        (mid-y (round (/ (glut:height window) 2))))
    (when *mouselook*
      ;; Without this check, glut:warp-pointer would cause endless recursion
      (unless (and (eq x mid-x) (eq y mid-y))
        (glut:warp-pointer mid-x mid-y)
        (rotate-camera (* (- x mid-x) *mouse-sensitivity*)
                       (* (- y mid-y) *mouse-sensitivity*))))))

(defmethod glut:keyboard ((window letcn-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\Tab (setf *shader-enabled* (not *shader-enabled*)))
    (#\b (toggle-blend))
    (#\m (toggle-mouselook window))
    (#\w (setf *forward-pressed* t))
    (#\s (setf *back-pressed* t))
    (#\a (setf *left-pressed* t))
    (#\d (setf *right-pressed* t))
    (#\1 (setf *selected-tool* 1))
    (#\2 (setf *selected-tool* 2))
    (#\3 (setf *selected-tool* 3))))

(defmethod glut:keyboard-up ((window letcn-window) key x y)
  (declare (ignore x y))
  (case key
    (#\w (setf *forward-pressed* nil))
    (#\s (setf *back-pressed* nil))
    (#\a (setf *left-pressed* nil))
    (#\d (setf *right-pressed* nil))))

(defmethod glut:reshape ((window letcn-window) width height)
  (let ((aspect-ratio (coerce (/ width height) 'single-float))
        (fov-y (deg-to-rad 60)))
    (setf *projection* (perspective-projection fov-y aspect-ratio 1.0 9000.0)))
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-matrix *projection*)
  (gl:matrix-mode :modelview))

(defun start ()
  (glut:display-window (make-instance 'letcn-window)))
