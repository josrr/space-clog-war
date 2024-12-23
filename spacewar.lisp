(in-package #:spacewar)

;;;;
(defclass obj ()
  (;;(pos :initarg :pos :reader pos)
   (x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)
   (box :initarg :box :reader box)))

(defclass star (obj)
  ())

(defclass missile (obj)
  ())

(defgeneric update (obj &optional event)
  (:documentation "Updates OBJ properties"))

(defgeneric draw (obj display)
  (:documentation "Draws OBJ in DISPLAY"))

;; (defgeneric read-input (obj event) (:documentation "Reads Input DISPLAY"))

(defmethod draw ((obj star) display)
  (labels ((random-step ()
             (- (random 1.8) 0.9)))
    (loop with bx = (random-step)
          with by = (random-step)
          repeat (+ 9 (random 8))
          for x = (x obj) then (+ x bx)
          for y = (y obj) then (+ y by)
          do (display:draw-point display x y))))
