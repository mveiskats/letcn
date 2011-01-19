(in-package :letcn)

(defconstant +golden-ratio+ (/ (+ 1 (sqrt 5)) 2))

(defclass fuzzy-sphere (3d-object)
  ((vertex-count
    :accessor sphere-vertex-count
    :initarg :vertex-count
    :documentation "Please keep even. Or else!")
   (vertex-arr
    :accessor sphere-vertex-arr
    :initarg :vertex-arr)
   (fuzz-wavelength
    :initarg :fuzz-wavelength)
   (fuzz-amplitude
    :initarg :fuzz-amplitude)
   (fuzz-lookup)))

;;; Please keep vertex count even. Or else!
(defun make-fuzzy-sphere (radius vertex-count)
  (make-instance 'fuzzy-sphere
                 :vertex-count vertex-count
                 :vertex-arr (make-sphere-vertices radius vertex-count)
                 :fuzz-wavelength (+ 50 (random 25))
                 :fuzz-amplitude (random 0.3)
                 :position '(0 0 0)
                 :rot-angle 0
                 :rot-vector '(1 0 0)))

(defmethod initialize-instance :after ((s fuzzy-sphere) &rest args)
  (declare (ignore args))
  (with-slots (fuzz-wavelength fuzz-amplitude fuzz-lookup) s
    (setf fuzz-lookup (make-array (list fuzz-wavelength) :element-type 'float :initial-element 0.0))
    (dotimes (i fuzz-wavelength)
      (setf (aref fuzz-lookup i)
            (1+ (* fuzz-amplitude (sin (* i (/ 2pi fuzz-wavelength)))))))))

(defmethod draw ((s fuzzy-sphere))
  (let* ((triangle-count (- (sphere-vertex-count s) 2))
         (index-count (* 2 triangle-count)))
    (cffi:with-foreign-objects ((v-arr '%gl:float (* (sphere-vertex-count s) 3))
                                (i-arr '%gl:uint index-count))
      ;; apply fuzz and dump vertex coordinates into gl:float array
      ;; fuzz is animated at 30 fps
      (let ((frame (truncate (get-internal-real-time)
                             (/ internal-time-units-per-second 30))))
        (with-slots (fuzz-wavelength fuzz-lookup) s
          (dotimes (i (* (sphere-vertex-count s) 3))
            (setf (cffi:mem-aref v-arr '%gl:float i)
                  (* (aref fuzz-lookup (mod (+ frame i) fuzz-wavelength))
                     (row-major-aref (sphere-vertex-arr s) i))))))

      ;; fill the indices array for the triangle fan
      (macrolet ((index-ref (i) `(cffi:mem-aref i-arr '%gl:uint ,i)))
        (dotimes (i (/ triangle-count 2))
          (let ((n (1- (sphere-vertex-count s))))
            (setf (index-ref (+ (* 4 i) 0)) i
                  (index-ref (+ (* 4 i) 1)) (1+ i)
                  (index-ref (+ (* 4 i) 2)) (- n i)
                  (index-ref (+ (* 4 i) 3)) (- n i 1)))))

      (let ((vert-buff (car (gl:gen-buffers 1))))
        (gl:bind-buffer :array-buffer vert-buff)
        (%gl:buffer-data :array-buffer
                         (* gl-float-size (sphere-vertex-count s) 3)
                         v-arr
                         :stream-draw)

        (gl:enable-client-state :vertex-array)
        (gl:enable-client-state :normal-array)

        (%gl:normal-pointer :float 0 (cffi:null-pointer))
        (%gl:vertex-pointer 3 :float 0 (cffi:null-pointer))
        (%gl:draw-elements :triangle-strip index-count :unsigned-int i-arr)

        (gl:disable-client-state :normal-array)
        (gl:disable-client-state :vertex-array)

        (gl:bind-buffer :array-buffer 0)
        (gl:delete-buffers (list vert-buff))))))

(defun make-sphere-vertices (radius n)
  (let ((vertices (make-array (list n 3))))
    (loop with delta-longitude = (/ (* 2 pi) +golden-ratio+)
          and delta-z = (/ (* 2 radius) n)
          and longitude = 0
          and z = (- radius)
          for i from 0 to (1- n)
          do (let* ((r (sqrt (- (* radius radius)
                                (* z z))))
                    (x (* r (cos longitude)))
                    (y (* r (sin longitude))))
               (setf (aref vertices i 0) x
                     (aref vertices i 1) y
                     (aref vertices i 2) z))
          do (incf longitude delta-longitude)
          do (incf z delta-z)
          do (setf longitude (normalize-angle longitude)))
    vertices))
