;;;; package.lisp

(defpackage #:display
  (:use #:cl)
  (:local-nicknames (#:gl #:clog-webgl))
  (:export #:clear
           #:draw
           #:draw-point
           #:draw-points
           #:height
           #:make-display
           #:width))

(defpackage #:expensive-planetarium
  (:use #:cl)
  (:export #:draw
           #:planetarium
           #:update
           #:x))

(defpackage #:spacewar
  (:use #:cl)
  (:export #:draw
           #:star))

(defpackage #:space-clog-war
  (:use #:cl #:clog #:clog-gui #:clog-webgl)
  (:export #:start))
