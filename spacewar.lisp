(in-package #:spacewar)

;;;;
(defparameter *display* nil)

(defclass obj ()
  ((x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)
   (box :initarg :box :reader box)))

(defclass toroidal (obj)
  ())

(defmethod (setf x) :after (new-value (object toroidal))
  (when *display*
    (cond ((> new-value (display:width/2 *display*))
           (setf (slot-value object 'x) (- new-value (display:width *display*))))
          ((< new-value (- (display:width/2 *display*)))
           (setf (slot-value object 'x) (+ new-value (display:width *display*)))))))

(defmethod (setf y) :after (new-value (object toroidal))
  (when *display*
    (cond ((> new-value (display:width/2 *display*))
           (setf (slot-value object 'y) (- new-value (display:width *display*))))
          ((< new-value (- (display:width/2 *display*)))
           (setf (slot-value object 'y) (+ new-value (display:width *display*)))))))

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
