(in-package #:spacewar)

;;;;
(defclass missile (explosible obj gravitational toroidal)
  ((lifetime :initform 120 :reader lifetime))
  (:default-initargs :dx 1.0 :dy 1.0))

(defun make-missile (x y dx dy)
  (make-instance 'missile
                 :x x :y y
                 :xm x :ym y
                 :dx dx :dy dy))

(defmethod update ((obj missile) context)
  (let ((*display* (display context)))
    (incf (x obj) (/ (dx obj) 8.0))
    (incf (y obj) (/ (dy obj) 8.0))))

(defmethod draw ((obj missile) context)
  (display:draw-point (display context) (x obj) (y obj) 1))
