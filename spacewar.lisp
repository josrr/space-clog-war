(in-package #:spacewar)

;;;;
(defparameter *display* nil)
(defparameter *gravity* 8.0)
(defparameter *collision-radius* 8.0)

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

(defgeneric update (obj display1)
  (:documentation "Updates OBJ properties"))

(defmethod update :before ((obj gravitational) display)
  (flet ((pof ()
           (setf (x obj) (1- (display:width/2 *display*))
                 (y obj) (1- (display:height/2 *display*))))
         (gravity (len)
           (let ((fxy (/ (expt (truncate (sqrt len)) 3) *gravity*)))
             (decf (x obj) (/ (x obj) fxy))
             (decf (y obj) (/ (y obj) fxy)))))
    (let ((*display* display)
          (len (+ (expt (/ (x obj) 8.0) 2.0) (expt (/ (y obj) 8.0) 2.0))))
      (if (>= len *collision-radius*)
          (gravity len)
          (pof)))))

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
