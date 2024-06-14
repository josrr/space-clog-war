;;;; package.lisp

(defpackage #:display
  (:use #:cl)
  (:local-nicknames (#:gl #:clog-webgl))
  (:export #:make-display
           #:clear
           #:draw
           #:draw-point
           #:draw-points))

(defpackage #:spacewar
  (:use #:cl)
  (:export #:star
           #:draw))

(defpackage #:space-clog-war
  (:use #:cl #:clog #:clog-gui #:clog-webgl)
  (:export #:start))
