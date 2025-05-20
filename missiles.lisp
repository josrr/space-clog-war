(in-package #:spacewar)

;;;;
(defclass missile (explosible obj gravitational toroidal)
  ((lifetime :initform 480 :accessor lifetime))
  (:default-initargs :dx 1.0 :dy 1.0))

(defun make-missile (x y dx dy)
  (make-instance 'missile
                 :x x :y y
                 :xm x :ym y
                 :dx dx :dy dy))

(defmethod update ((obj missile) context)
  (let ((*display* (display context)))
    (incf (x obj) (/ (dx obj) 8.0))
    (incf (y obj) (/ (dy obj) 8.0))
    (if (zerop (lifetime obj))
        (setf (objects context) (remove obj (objects context)))
        (decf (lifetime obj)))))

(defmethod draw ((obj missile) display)
  (display:draw-point display (x obj) (y obj) 1))
