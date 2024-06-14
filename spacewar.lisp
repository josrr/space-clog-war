(in-package #:spacewar)

;;;;
(defclass obj ()
  ((pos :initarg :pos :reader pos)
   (box :initarg :box :reader box)))

(defclass star (obj)
  ())

(defclass ship (obj)
  ())

(defclass missile (obj)
  ())

(defgeneric draw (obj display)
  (:documentation "Draws OBJ in DISPLAY"))

(defmethod draw ((obj star) display)
  (labels ((random-step ()
             (- (random 1.8) 0.9)))
    (loop with bx = (random-step)
          with by = (random-step)
          repeat (+ 9 (random 8))
          for x = 0.0 then (+ x bx)
          for y = 0.0 then (+ y by)
          do (display:draw-point display x y))))
