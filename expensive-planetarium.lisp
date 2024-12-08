(in-package #:expensive-planetarium)

(defparameter *star-map-width* 8192)

(defclass star ()
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)
   (name :initarg :name :reader name)
   (constellation :initarg :constellation :reader constellation)
   (magnitude :initarg :magnitude :reader magnitude)))

(defun load-stars (filename)
  (with-open-file (stream filename :direction :input)
    (sort (loop for group in (read stream)
                append (mapcar (lambda (star)
                                 (make-instance 'star
                                                :x (- *star-map-width* (first star))
                                                :y (second star)
                                                :name (fifth star)
                                                :constellation (fourth star)
                                                :magnitude (car group)))
                               (cdr group)))
          #'> :key #'x)))

(defparameter *stars* (load-stars #P"stars-data.lisp"))

(defclass planetarium (spacewar:obj)
  ((stars :initform *stars* :reader stars)
   (star-map-width :initform *star-map-width* :reader star-map-width)
   (counter :initform 0 :accessor counter))
  (:default-initargs :x (* *star-map-width* 1/2)))

#|
(defmethod spacewar:draw ((planetarium planetarium) display)
  (when (zerop (mod (counter planetarium) 2))
    (dolist (star (stars planetarium))
      (let ((x (- (x star) (x planetarium))))
        (when (plusp x)
          (decf x (star-map-width planetarium)))
        (when (and (minusp x) (plusp (incf x (display:width display))))
          (display:draw-point display (- x (display:width/2 display)) (y star)
                              (- 4 (magnitude star))))))))
|#

(defmethod spacewar:draw ((planetarium planetarium) display)
  (when (zerop (mod (counter planetarium) 2))
    (dolist (star (stars planetarium))
      (let ((x (- (x star) (spacewar:x planetarium))))
        (when (plusp x)
          (decf x (star-map-width planetarium)))
        (when (and (minusp x) (plusp (incf x (display:width display))))
          (display:draw-point display (- x (display:width/2 display)) (y star)
                              (- 4 (magnitude star))))))))

(defmethod spacewar:update ((planetarium planetarium))
  (with-accessors ((x spacewar:x) (counter counter)) planetarium
    (incf counter)
    (when (= 20 counter)
      (setf counter 0)
      (decf x)
      (when (minusp x)
        (setf x (star-map-width planetarium))))))
