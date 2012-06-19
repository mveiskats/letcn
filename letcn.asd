;;;; -*- Mode: Lisp -*-

(defsystem :letcn
  :depends-on (:cl-opengl :cl-glut :sb-cga)
  :components ((:file "package")
               (:file "matrix-stack" :depends-on ("package"))
               (:file "utility" :depends-on ("package" "matrix-stack"))
               (:file "platonic-solids" :depends-on ("package"))
               (:file "quaternion" :depends-on ("utility"))
               (:file "space" :depends-on ("quaternion"))
               (:file "scene" :depends-on ("hyperboloid"
                                           "honeycomb/octree"))
               (:file "hyperboloid" :depends-on ("utility"))
               (:file "fuzzy-sphere" :depends-on ("utility" "space"))
               (:file "simplex-noise" :depends-on ("utility"))
               (:file "collision" :depends-on ("utility"))
               (:file "honeycomb/lattice" :depends-on ("utility"
                                                       "collision"))
               (:file "honeycomb/cell" :depends-on ("honeycomb/lattice"))
               (:file "honeycomb/octree" :depends-on ("honeycomb/cell"
                                                      "space"
                                                      "simplex-noise"))
               (:file "honeycomb/collision" :depends-on ("honeycomb/octree"))
               (:file "shaders" :depends-on ("package"))
               (:file "init" :depends-on ("platonic-solids"
                                          "fuzzy-sphere"
                                          "hyperboloid"
                                          "honeycomb/octree"
                                          "scene"
                                          "shaders"))
               (:file "tests/main" :depends-on ("init"))))
