(in-package :letcn)

(defun hyperboloid-vertices (a c)
   (let ((result (make-array '(100 3))))
     (flet ((param-x (u v) (* a (- (cos u) (* v (sin u)))))
            (param-y (u v) (* a (+ (sin u) (* v (cos u)))))
            (param-z (u v) (declare (ignore u)) (* c v)))
       (dotimes (i 10)
         (dotimes (j 10)
           (let ((u (* j (/ pi 5)))
                 (v (- i 4)))
             (setf (aref result (+ i (* j 10)) 0) (param-x u v)
                   (aref result (+ i (* j 10)) 1) (param-y u v)
                   (aref result (+ i (* j 10)) 2) (param-z u v))))))
     result))


