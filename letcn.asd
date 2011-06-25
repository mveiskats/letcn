;;;; -*- Mode: Lisp -*-

(defsystem :letcn
  :depends-on (:cl-opengl :cl-glut)
  :components ((:file "package")
               (:file "utility" :depends-on ("package"))
               (:file "matrix" :depends-on ("package"))
               (:file "platonic-solids" :depends-on ("package"))
               (:file "space" :depends-on ("package"))
               (:file "scene" :depends-on ("matrix"
                                           "hyperboloid"
                                           "lattice"
                                           "honeycomb"))
               (:file "hyperboloid" :depends-on ("utility"))
               (:file "fuzzy-sphere" :depends-on ("utility" "space"))
               (:file "simplex-noise" :depends-on ("utility"))
               (:file "lattice" :depends-on ("simplex-noise"))
               (:file "collision" :depends-on ("utility"))
               (:file "honeycomb" :depends-on ("collision"
                                               "space"
                                               "simplex-noise"))
               (:file "init" :depends-on ("platonic-solids"
                                          "fuzzy-sphere"
                                          "hyperboloid"
                                          "lattice"
                                          "honeycomb"
                                          "scene"))))