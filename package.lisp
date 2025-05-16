;;;; package.lisp

(defpackage #:display
  (:use #:cl)
  (:local-nicknames (#:gl #:clog-webgl))
  (:export #:clear
           #:draw
           #:draw-point
           #:draw-points
           #:height
           #:height/2
           #:make-display
           #:width
           #:width/2))

(defpackage #:expensive-planetarium
  (:use #:cl)
  (:export #:planetarium))

(defpackage #:spacewar
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:draw
           #:update
           #:star
           #:obj
           #:toroidal
           #:x
           #:ot1
           #:ot2
           #:left
           #:right
           #:up
           #:down))

(defpackage #:space-clog-war
  (:use #:cl #:clog #:clog-gui #:clog-webgl)
  (:export #:start))
