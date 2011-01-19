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

(defvar *mouse-sensitivity* 0.0002) ;; Pixel to radian ratio
(defvar *move-speed* 3)

(defclass letcn-window (glut:window)
  (scene camera)
  (:default-initargs
   :title "OMG, work already!"
   :mode '(:double :rgb)
   :width 800
   :height 600))

(defmethod glut:display-window :before ((window letcn-window))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (setf (slot-value window 'scene) (make-scene))
  (setf (slot-value window 'camera)
        (make-instance 'camera :position #(0.0 0.0 -20.0))))

(defun draw-triangle-list (tri-list)
  (gl:with-primitives :triangles
    (dolist (triangle tri-list)
      (dolist (vertex triangle)
        (apply #'gl:normal vertex)
        (apply #'gl:vertex vertex)))))

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
  (block draw
    (let ((current-t (get-internal-real-time)))
      (unless *old-t* (setf *old-t* current-t))
      (setf *delta-t* (- current-t *old-t*)
            *old-t* current-t))

    (gl:clear :color-buffer :depth-buffer)
    (gl:light :light0 :position #(0.5 0.5 0.5 1.0))
    (gl:enable :lighting :light0 :depth-test :color-material)
    (gl:disable :cull-face)
    (gl:blend-func :src-alpha :one)

    (with-slots (scene camera) window
      (restart-case (draw-scene scene camera)
                    (quit-gracefully ()
                      (glut:destroy-current-window)
                      (return-from draw)))
      (let (move-directions)
        (when *forward-pressed* (push #(0.0 0.0 1.0) move-directions))
        (when *back-pressed* (push #(0.0 0.0 -1.0) move-directions))
        (when *left-pressed* (push #(1.0 0.0 0.0) move-directions))
        (when *right-pressed* (push #(-1.0 0.0 0.0) move-directions))
        (when move-directions
          (move-camera camera
                       (map 'vector
                            (lambda (a)
                              (* a *move-speed*
                                 (/ *delta-t* internal-time-units-per-second)))
                            (apply #'map `(vector ,#'+ . ,move-directions)))))))

    (glut:swap-buffers)
    (glut:post-redisplay)
    (count-fps)))

(defmethod glut:mouse ((window letcn-window) button state x y)
  (case button
    (:left-button (setf *left-mouse-down* (eq state :down)))
    (:righ-button (setf *right-mouse-down* (eq state :down)))))

(defmethod glut:passive-motion ((window letcn-window) x y)
  (let ((mid-x (round (/ (glut:width window) 2)))
        (mid-y (round (/ (glut:height window) 2))))
    (when *mouselook*
      ;; Without this check, glut:warp-pointer would cause endless recursion
      (unless (and (eq x mid-x) (eq y mid-y))
        (glut:warp-pointer mid-x mid-y)
        (rotate-camera (slot-value window 'camera)
                       (* (- mid-x x) *mouse-sensitivity*)
                       (* (- mid-y y) *mouse-sensitivity*))))))

(defun move-camera (camera direction)
  (with-slots (position rotation) camera
    (setf position (map 'vector #'+
                        (matrix-product rotation direction)
                        position))))

(defmethod glut:keyboard ((window letcn-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\b (toggle-blend))
    (#\m (toggle-mouselook window))
    (#\w (setf *forward-pressed* t))
    (#\s (setf *back-pressed* t))
    (#\a (setf *left-pressed* t))
    (#\d (setf *right-pressed* t))))

(defmethod glut:keyboard-up ((window letcn-window) key x y)
  (declare (ignore x y))
  (case key
    (#\w (setf *forward-pressed* nil))
    (#\s (setf *back-pressed* nil))
    (#\a (setf *left-pressed* nil))
    (#\d (setf *right-pressed* nil))))

(defmethod glut:reshape ((window letcn-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((h (/ height width)))
    (gl:frustum -1 1 (- h) h 2 9000))
  (gl:matrix-mode :modelview))

(defun start ()
  (glut:display-window (make-instance 'letcn-window)))
