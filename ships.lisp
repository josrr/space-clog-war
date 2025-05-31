(in-package #:spacewar)

;;;;
(defclass steerable ()
  ((pace :initarg :pace :initform (/ 14.0) :accessor pace)
   (fuel :initform 1000 :accessor fuel)
   (shots :initform 10 :accessor shots)
   (previous-shot :initform nil :accessor previous-shot)))

(defmethod spacewar:update :before ((obj steerable) context)
  (declare (ignore context))
  (when (left obj)
    (decf (theta obj) 0.025))
  (when (right obj)
    (incf (theta obj) 0.025))
  (when (and (down obj) (> (fuel obj) 0))
    (decf (fuel obj))
    (decf (bx obj) (* (sine obj) (pace obj)))
    (decf (by obj) (* (cosine obj) (pace obj))))
  (let ((now (local-time:now)))
    (when (and (up obj) (or (null (previous-shot obj))
                            (> (local-time:timestamp-difference now (previous-shot obj))
                               0.25))
               (> (shots obj) 0))
      (setf (previous-shot obj) now)
      (decf (shots obj))
      (push (make-missile (- (x obj) (* 8.0 (sine obj)))
                          (- (y obj) (* 8.0 (cosine obj)))
                          (- (dx obj) (* 8.0 (sine obj)))
                          (- (dy obj) (* 8.0 (cosine obj))))
            (new-objects obj)))))

;;;
(defclass ship (explosible obj gravitational steerable toroidal)
  ((size :initform 1.0 :accessor size)
   (saved-x :initarg :saved-x :initform nil :accessor saved-x)
   (saved-y :initarg :saved-y :initform nil :accessor saved-y)
   (xx :initform 0.0 :accessor xx)
   (yy :initform 0.0 :accessor yy)
   (flipped :initarg :flipped :initform nil :accessor flipped-p)
   (theta :initarg :theta :initform 0.0 :accessor theta)
   (sin :initform (sin 0.0) :accessor sine)
   (cos :initform (cos 0.0) :accessor cosine)
   (ssn :initform nil :accessor ssn)
   (scn :initform nil :accessor scn)
   (ssm :initform nil :accessor ssm)
   (scm :initform nil :accessor scm)
   (ssc :initform nil :accessor ssc)
   (csn :initform nil :accessor csn)
   (ssd :initform nil :accessor ssd)
   (csm :initform nil :accessor csm)
   (left :initform nil :accessor left)
   (right :initform nil :accessor right)
   (up :initform nil :accessor up)
   (down :initform nil :accessor down)))

(defmethod (setf x) :after (new-value (object ship))
  (setf (slot-value object 'xx) new-value))

(defmethod (setf y) :after (new-value (object ship))
  (setf (slot-value object 'yy) new-value))

(defmethod (setf theta) :after (theta (object ship))
  (with-accessors ((sine sine)
                   (cosine cosine)
                   (ssn ssn) (scn scn)
                   (ssm ssm) (scm scm)
                   (ssc ssc) (csn csn)
                   (ssd ssd) (csm csm))
      object
    (setf sine (sin theta)
          cosine (cos theta)
          ssn (* (size object) sine)
          scn (* (size object) cosine)
          ssm (if (flipped-p object) (- ssn) ssn)
          scm (if (flipped-p object) (- scn) scn)
          ssc (+ ssn scm)
          csn (- ssn scm)
          ssd (+ scn ssm)
          csm (- scn ssm))))

(defmethod (setf flipped-p) :after (value (object ship))
  (with-accessors ((ssn ssn) (scn scn)
                   (ssm ssm) (scm scm)
                   (ssc ssc) (csn csn)
                   (ssd ssd) (csm csm))
      object
    (setf ssm (if (flipped-p object) (- ssn) ssn)
          scm (if (flipped-p object) (- scn) scn)
          ssc (+ ssn scm)
          csn (- ssn scm)
          ssd (+ scn ssm)
          csm (- scn ssm))))

(defmethod initialize-instance :after ((obj ship) &rest initargs &key &allow-other-keys)
  (setf (theta obj) (getf initargs :theta 0.0)
        (xx obj) (getf initargs :x 0.0)
        (yy obj) (getf initargs :y 0.0)
        (xm obj) (xx obj)
        (ym obj) (yy obj)))

(defgeneric draw-point (obj display direction)
  (:method ((obj ship) display (direction (eql :down)))
    (declare (ignore display))
    (incf (xx obj) (ssn obj))
    (incf (yy obj) (scn obj)))
  (:method ((obj ship) display (direction (eql :out)))
    (declare (ignore display))
    (incf (xx obj) (scm obj))
    (decf (yy obj) (ssm obj)))
  (:method ((obj ship) display (direction (eql :out-down)))
    (declare (ignore display))
    (incf (xx obj) (ssc obj))
    (incf (yy obj) (csm obj)))
  (:method ((obj ship) display (direction (eql :in)))
    (declare (ignore display))
    (decf (xx obj) (scm obj))
    (incf (yy obj) (ssm obj)))
  (:method ((obj ship) display (direction (eql :in-down)))
    (declare (ignore display))
    (incf (xx obj) (csn obj))
    (incf (yy obj) (ssd obj)))
  (:method :after ((obj ship) display direction)
    (declare (ignore direction))
    (display:draw-point display (xx obj) (yy obj))))

(defgeneric save/restore (obj)
  (:method ((obj ship))
    (if (and (saved-x obj) (saved-y obj))
        (setf (xx obj) (saved-x obj)
              (yy obj) (saved-y obj)
              (saved-x obj) nil
              (saved-y obj) nil)
        (setf (saved-x obj) (xx obj)
              (saved-y obj) (yy obj)))))

(defgeneric flip (obj display)
  (:method ((obj ship) display)
    (flet ((draw-exhaust ()
             (loop repeat (+ 5 (random 15))
                   for x = (xx obj) then (+ x (sine obj))
                   for y = (yy obj) then (+ y (cosine obj))
                   do (display:draw-point display x y 0))))
      (setf (flipped-p obj) (not (flipped-p obj)))
      (when (flipped-p obj)
        (setf (width obj) (- (xx obj) (x obj))
              (height obj) (- (yy obj) (y obj)))
        (when (and (down obj) (> (fuel obj) 0))
          (draw-exhaust)))
      (setf (saved-x obj) nil
            (saved-y obj) nil
            (xx obj) (x obj)
            (yy obj) (y obj))
      (when (flipped-p obj)
        (spacewar:draw obj display)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun generate-instruction (byte)
    (cond ((< byte 6) (values (case byte
                                ((0 1) `(draw-point obj display :down))
                                (2 `(draw-point obj display :out))
                                (3 `(draw-point obj display :out-down))
                                (4 `(draw-point obj display :in))
                                (5 `(draw-point obj display :in-down)))
                              nil))
          (t (case byte
               (6 (values `(save/restore obj) nil))
               (7 (values `(flip obj display) t)))))))

(defmacro compile-outline (name (&rest encoding))
  `(progn
     (defclass ,name (ship) ())
     (defmethod spacewar:draw ((obj ,name) display)
       ,@(loop for word in encoding
               append (loop for pos = 15 then (- pos 3)
                            for byte = (byte 3 pos)
                            while (>= pos 0)
                            for (instruction end-p) = (multiple-value-list
                                                       (generate-instruction (ldb byte word)))
                            if instruction
                              collect instruction
                            until end-p)))))

(compile-outline ot1 (#o111131
                      #o111111
                      #o111111
                      #o111163
                      #o311111
                      #o146111
                      #o111114
                      #o700000))

(compile-outline ot2 (#o013113
                      #o113111
                      #o116313
                      #o131111
                      #o161151
                      #o111633
                      #o365114
                      #o700000))

(defmethod spacewar:update ((obj ship) context)
  (let ((*display* (display context)))
    (incf (dx obj) (bx obj))
    (incf (dy obj) (by obj))
    (incf (x obj) (/ (dx obj) 8.0))
    (incf (y obj) (/ (dy obj) 8.0))))

(defmethod explode :after ((obj ship) display)
  (setf (dx obj) 0.0 (dy obj) 0.0
        (bx obj) 0.0 (by obj) 0.0
        (x obj) (1- (display:width/2 display))
        (y obj) (1- (display:height/2 display))))
