;;;; -*- Mode: Lisp -*-

(defsystem :letcn
  :depends-on (:cl-opengl :cl-glut :sb-cga)
  :components ((:file "package")
               (:file "matrix-stack" :depends-on ("package"))
               (:file "utility" :depends-on ("package" "matrix-stack"))
               (:file "platonic-solids" :depends-on ("package"))
               (:file "space" :depends-on ("package"))
               (:file "scene" :depends-on ("hyperboloid"
                                           "honeycomb"))
               (:file "hyperboloid" :depends-on ("utility"))
               (:file "fuzzy-sphere" :depends-on ("utility" "space"))
               (:file "simplex-noise" :depends-on ("utility"))
               (:file "collision" :depends-on ("utility"))
               (:file "truncated-octahedron" :depends-on ("utility"))
               (:file "honeycomb" :depends-on ("collision"
                                               "space"
                                               "simplex-noise"
                                               "truncated-octahedron"))
               (:file "shaders" :depends-on ("package"))
               (:file "init" :depends-on ("platonic-solids"
                                          "fuzzy-sphere"
                                          "hyperboloid"
                                          "honeycomb"
                                          "scene"))))
