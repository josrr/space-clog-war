(in-package #:spacewar)

;;;;
(defparameter *display* nil)
(defparameter *gravity* 8.0)
(defparameter *collision-radius* 5.0)

(defclass context ()
  ((display :initarg :display :reader display)
   (objects :initarg :objects :accessor objects)))

(defclass obj ()
  ((x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)
   (xm :initarg :xm :initform 0.0 :accessor xm)
   (ym :initarg :ym :initform 0.0 :accessor ym)
   (dx :initarg :dx :initform 0.0 :accessor dx)
   (dy :initarg :dy :initform 0.0 :accessor dy)
   (bx :initarg :dx :initform 0.0 :accessor bx)
   (by :initarg :dy :initform 0.0 :accessor by)
   (width :initform 0.0 :accessor width)
   (height :initform 0.0 :accessor height)
   (box :initarg :box :reader box)
   (new-objects :initform nil :accessor new-objects)))

(defmethod (setf x) :after (new-value (object obj))
  (setf (slot-value object 'xm) (+ new-value (/ (width object) 2.0))))

(defmethod (setf y) :after (new-value (object obj))
  (setf (slot-value object 'ym) (+ new-value (/ (height object) 2.0))))

(defclass toroidal ()
  ())

(defclass gravitational ()
  ())

(defclass explosible ()
  ((duration :initarg duration :initform 60 :reader duration)
   (particles :initarg particles :initform 20 :reader particles)
   (radius :initarg radius :initform 128.0 :reader radius)
   (explode :initform nil :accessor explodep)))

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

(defgeneric update (obj context)
  (:documentation "Updates OBJ properties"))

(defgeneric explode (obj display)
  (:documentation "Explodes OBJ"))

(defmethod explode ((obj obj) display)
  (declare (ignore obj display)))

(defmethod explode ((obj explosible) display)
  (declare (ignore display))
  (setf (explodep obj) t))

(defmethod update :before ((obj gravitational) context)
  (flet ((gravity (len)
           (let ((g (/ (* len (sqrt len)) 2.0)))
             (setf (bx obj) (- (/ (x obj) g)))
             (setf (by obj) (- (/ (y obj) g))))))
    (let ((*display* (display context))
          (len (+ (expt (/ (xm obj) *gravity*) 2.0)
                  (expt (/ (ym obj) *gravity*) 2.0))))
      (if (< len *collision-radius*)
          (explode obj (display context))
          (gravity len)))))

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

(defmethod draw :around ((obj explosible) display)
  (flet ((draw-particles (duration)
           (lambda ()
             (loop repeat (particles obj)
                   for factor = (random (radius obj))
                   do (display:draw-point display
                                          (+ (xm obj)
                                             (* factor (- (random 1.0) 0.5)))
                                          (+ (ym obj)
                                             (* factor (- (random 1.0) 0.5)))
                                          2))
             (when (zerop (decf duration))
               (setf (explodep obj) nil)))))
    (cond ((explodep obj)
           (unless (typep (explodep obj) 'function)
             (setf (explodep obj) (draw-particles (duration obj))))
           (funcall (explodep obj)))
          (t (call-next-method)))))

(defgeneric collisionp (a b)
  (:documentation "Detect collision between A and B"))

(defmethod collisionp ((a explosible) (b explosible))
  (let ((distance (+ (expt (- (xm a) (xm b)) 2)
                     (expt (- (ym a) (ym b)) 2))))
    (< distance 45.0)))

(defgeneric detect-collisions (context))

(defmethod detect-collisions ((context context))
  (loop with objects = (remove-if-not (lambda (obj)
                                        (typep obj 'explosible))
                                      (objects context))
        for a in objects
        do (loop for b in objects
                 if (and (not (eq a b)) (collisionp a b))
                   do (explode a (display context))
                      (explode b (display context)))))
