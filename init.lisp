(in-package :letcn)

(defparameter *blend* nil)
(defparameter *mouselook* nil)
(defparameter *left-mouse-down* nil)
(defparameter *right-mouse-down* nil)

(defparameter *forward-pressed* nil)
(defparameter *back-pressed* nil)
(defparameter *left-pressed* nil)
(defparameter *right-pressed* nil)

(defparameter *old-ticks* nil)
(defparameter *delta-ticks* nil)

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

(defmethod glut:display-window :before ((window letcn-window))
  (format t "OpenGL: ~a~%" (gl:get-string :version))
  (format t "Shaders: ~a~%" (gl:get-string :shading-language-version))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (make-scene)
  (setf *camera* (make-instance 'camera :position (vec 0.0 0.0 20.0)))

  (make-shader-program))

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
       (let ((current-ticks (get-internal-real-time)))
         (unless *old-ticks* (setf *old-ticks* current-ticks))
         (setf *delta-ticks* (- current-ticks *old-ticks*)
               *old-ticks* current-ticks))

       (gl:clear :color-buffer :depth-buffer)
       (gl:light :light0 :position #(-10.0 10.0 10.0 1.0))
       (gl:enable :lighting :light0 :depth-test :color-material)
       (gl:enable :cull-face)
       (gl:front-face :ccw)
       (gl:blend-func :src-alpha :one)

       (let ((delta-time (coerce (/ *delta-ticks*
                                    internal-time-units-per-second)
                                 'single-float)))
         (let (move-directions)
           (when *forward-pressed* (push +front+ move-directions))
           (when *back-pressed* (push +back+ move-directions))
           (when *left-pressed* (push +left+ move-directions))
           (when *right-pressed* (push +right+ move-directions))
           (when move-directions
             (move-camera (vec* (reduce #'vec+ move-directions
                                        :initial-value (vec 0.0 0.0 0.0))
                                (* *move-speed* delta-time)))))
         (pass-time delta-time))
       (draw-scene)

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
