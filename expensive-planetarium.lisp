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

(defclass planetarium ()
  ((stars :initform *stars* :reader stars)
   (x :initform (* *star-map-width* 1/2) :accessor x)
   (counter :initform 0 :accessor counter)))

(defgeneric draw (planetarium display)
  (:method ((planetarium planetarium) display)
    (when (zerop (mod (counter planetarium) 2))
      (let ((w/2 (/ (display:width display) 2)))
        (loop for star in (stars planetarium)
              for x = (- (x star) (x planetarium))
              with drawn-p = nil
              if (plusp x) do
                (decf x *star-map-width*)
              if (> (incf x (display:width display)) 0) do
                (display:draw-point display (float (- x w/2)) (float (y star))
                                    (- 4 (magnitude star)))
                (setf drawn-p t)
              else if drawn-p do (return))))))

(defgeneric update (planetarium)
  (:method ((planetarium planetarium))
    (with-accessors ((x x) (counter counter)) planetarium
      (incf counter)
      (when (= 20 counter)
        (setf counter 0)
        (decf x)
        (when (minusp x)
          (setf x *star-map-width*))))))
