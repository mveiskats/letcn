(in-package :letcn)

(defclass 3d-object ()
  ((position :initarg :position :initform (vec 0.0 0.0 0.0))
   (velocity :initarg :velocity :initform (vec 0.0 0.0 0.0))
   (rotation :initform +identity-quat+ :initarg :rotation))
  (:documentation "Object with local-to-world position and orientation"))

;;; Rotation accessors
(defun l2w-rotation (obj)
  (slot-value obj 'rotation))

(defun (setf l2w-rotation) (rot obj)
  (setf (slot-value obj 'rotation) rot))

(defun w2l-rotation (obj)
  (quat-inverse (l2w-rotation obj)))

(defun (setf w2l-rotation) (rot obj)
  (setf (l2w-rotation obj) (quat-inverse rot)))

;;; Rotation and transformation functions
(defun l2w-rotate (obj v) (quat-rotate v (l2w-rotation obj)))
(defun w2l-rotate (obj v) (quat-rotate v (w2l-rotation obj)))

(defun l2w-transform (obj v)
  (vec+ v (l2w-rotate obj (slot-value obj 'position))))

(defun w2l-transform (obj v)
  (w2l-rotate obj (vec- v (slot-value obj 'position))))

(defun lw2-transform-matrix (obj)
  (matrix* (translate (slot-value obj 'position))
           (quat-to-matrix (l2w-rotation obj))))

(defun w2l-transform-matrix (obj)
  (matrix* (quat-to-matrix (w2l-rotation obj))
           (translate (vec* (slot-value obj 'position) -1.0))))

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
(defmethod draw :around ((obj 3d-object))
  (with-transformation (translate (slot-value obj 'position))
    (call-next-method)))
