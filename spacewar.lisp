(in-package #:spacewar)

;;;;
(defparameter *display* nil)

(defclass obj ()
  ((x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)
   (box :initarg :box :reader box)))

(defclass toroidal (obj)
  ())

(defclass gravitational (obj)
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

(defgeneric on-key (obj key)
  (:documentation "Runs when KEY is pressed"))

(defgeneric update (obj display1)
  (:documentation "Updates OBJ properties"))

(defparameter *gravity* (/ 8.0))
(defparameter *collision-radius* 1.0)
(defun fxy (x y)
  (let ((len (+ (expt (/ x 8.0) 2.0) (expt (/ y 8.0) 2.0))))
    (let ((fxy (/ (expt (sqrt len) 3.0) 8.0)))
      fxy)))

(defmethod update :before ((obj gravitational) display)
  (flet ((fxy (x y)
           (let ((len (+ (expt (/ x 8.0) 2.0) (expt (/ y 8.0) 2.0))))
             (let ((fxy (/ (expt (sqrt len) 3.0) 8.0)))
               fxy))))
    (let ((*display* display))
      (let ((fxy (fxy (x obj) (y obj))))
        (decf (x obj) (/ (* 8.0 (x obj)) fxy))
        (decf (y obj) (/ (* 8.0 (y obj)) fxy))))))

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
