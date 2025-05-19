(in-package #:spacewar)

;;;;
(defclass missile (toroidal gravitational explosible) ()
  (:default-initargs :dx 1.0 :dy 1.0))

(defun make-missile (x y dx dy)
  (make-instance 'missile
                 :x x :y y
                 :dx (* 10 dx) :dy (* 10 dy)))

(defmethod update ((obj missile) display)
  (let ((*display* display))
    (decf (x obj) (dx obj))
    (decf (y obj) (dy obj))))

(defmethod draw ((obj missile) display)
  (display:draw-point display (x obj) (y obj) 1))
