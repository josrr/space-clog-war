(in-package #:spacewar)

;;;;
(defclass obj ()
  ((x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)
   (box :initarg :box :reader box)))

(defclass toroidal (obj)
  ())

(defmethod (setf x) :after (new-value (object toroidal))
  (cond ((> new-value 512)
         (setf (slot-value object 'x) (- new-value 1024)))
        ((< new-value -512)
         (setf (slot-value object 'x) (+ new-value 1024)))))

(defmethod (setf y) :after (new-value (object toroidal))
  (cond ((> new-value 512)
         (setf (slot-value object 'y) (- new-value 1024)))
        ((< new-value -512)
         (setf (slot-value object 'y) (+ new-value 1024)))))

(defclass star (obj)
  ())

(defclass missile (toroidal)
  ())

(defgeneric update (obj &optional event)
  (:documentation "Updates OBJ properties"))

(defgeneric draw (obj display)
  (:documentation "Draws OBJ in DISPLAY"))

(defmethod draw ((obj star) display)
  (labels ((random-step ()
             (- (random 1.8) 0.9)))
    (loop with bx = (random-step)
          with by = (random-step)
          repeat (+ 9 (random 8))
          for x = (x obj) then (+ x bx)
          for y = (y obj) then (+ y by)
          do (display:draw-point display x y))))
