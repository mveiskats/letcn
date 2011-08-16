(in-package :letcn)

(defclass 3d-object ()
  ((position :initarg :position :initform (vec 0.0 0.0 0.0)))
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
  (with-slots (position) object
    (with-transformation (translate position)
      (call-next-method))))

