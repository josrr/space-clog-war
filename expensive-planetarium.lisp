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
    (loop for group in (read stream)
          append (mapcar (lambda (star)
                           (make-instance 'star
                                          :x (- *star-map-width* (first star))
                                          :y (second star)
                                          :name (fourth star)
                                          :constellation (fifth star)
                                          :magnitude (car group)))
                         (cdr group)))))


(defparameter *stars* (load-stars #P"stars-data.lisp"))

(defclass planetarium ()
  ((stars :initform *stars* :reader stars)
   (x :initform (- (* *star-map-width* 1/2) 512) :accessor x)
   (counter :initform 0 :accessor counter)))

(defgeneric draw (planetarium display)
  (:method ((planetarium planetarium) display)
    (dolist (star (remove-if (complement
                              (lambda (star)
                                (<= (- (x planetarium) 512) (x star) (+ 1024 (x planetarium)))))
                             (stars planetarium)))
      (display:draw-point display (float (- (x star) (x planetarium))) (float (y star))))))

(defgeneric update (planetarium )
  (:method ((planetarium planetarium))
    (with-accessors ((x x) (counter counter)) planetarium
      (incf counter)
      (when (= 20 counter)
        (setf counter 0)
        (decf x)
        (when (< x 0)
          (setf x *star-map-width*))))))
