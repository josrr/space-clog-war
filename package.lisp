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
           #:width
           #:width/2))

(defpackage #:expensive-planetarium
  (:use #:cl)
  (:export ;; #:draw
           #:planetarium
           ;;#:update
           ;;#:x
           ))

(defpackage #:spacewar
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:draw
           #:update
           #:star
           #:obj
           #:x))

(defpackage #:space-clog-war
  (:use #:cl #:clog #:clog-gui #:clog-webgl)
  (:export #:start))
