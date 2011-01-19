(in-package :letcn)

(defclass 3d-object ()
  ((position :initarg :position :initform '(0 0 0))
   (rot-angle :initarg :rot-angle :initform 0)
   (rot-vector :initarg :rot-vector :initform '(1 0 0)))
  (:documentation "Object with position and orientation"))

(defgeneric draw (obj))

;;; Assume array is vertex list and draw as points
(defmethod draw ((arr array))
  (gl:color 1 1 1 1)
  (gl:point-size 5)
  (gl:with-primitives :points
    (dotimes (i (array-dimension arr 0))
      (gl:vertex (aref arr i 0)
                 (aref arr i 1)
                 (aref arr i 2)))))

;;; Just setting up the translations
(defmethod draw :around ((object 3d-object))
  (gl:push-matrix)
  (apply #'gl:translate (slot-value object 'position))
  (apply #'gl:rotate (cons (slot-value object 'rot-angle)
                           (slot-value object 'rot-vector)))
  (call-next-method)
  (gl:pop-matrix))

