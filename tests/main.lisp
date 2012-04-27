(in-package :letcn)

(defun test-equal (expected received)
  (unless (equal expected received)
    (format t "Expected ~a but received ~a~%" expected received)))

(defun test-rasterize-segment (x0 y0 z0 x1 y1 z1)
  (let (foo)
    (rasterize-segment x0 y0 z0 x1 y1 z1
                       (lambda (x y z) (push (list x y z) foo)))
    (reverse foo)))

(defun run-tests ()
  (let ((inputs '((0.5 0.5 0.5 2.5 2.5 2.5)
                  (2.5 2.5 2.5 0.5 0.5 0.5)
                  (0.5 0.5 0.5 2.5 1.5 0.5)
                  (2.5 1.5 0.5 0.5 0.5 0.5)))
        (outputs '(((0 0 0) (1 1 1) (2 2 2))
                   ((2 2 2) (1 1 1) (0 0 0))
                   ((0 0 0) (1 0 0) (1 1 0) (2 1 0))
                   ((2 1 0) (1 1 0) (1 0 0) (0 0 0)))))
    (mapc #'test-equal
          outputs
          (mapcar (lambda (a) (apply #'test-rasterize-segment a)) inputs)))
  (values))
