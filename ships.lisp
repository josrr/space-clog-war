(in-package #:spacewar)

(defclass ship (obj)
  ((size :initform 1.0 :accessor size)
   (x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)
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
   (csm :initform nil :accessor csm)))

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
        (yy obj) (getf initargs :y 0.0)))

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

(defgeneric flip (obj)
  (:method ((obj ship))
    (setf (xx obj) (x obj)
          (yy obj) (y obj)
          (saved-x obj) nil
          (saved-y obj) nil
          (flipped-p obj) (not (flipped-p obj)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun generate-instruction (byte)
    (cond ((< byte 6) (case byte
                        ((0 1) `(draw-point obj display :down))
                        (2 `(draw-point obj display :out))
                        (3 `(draw-point obj display :out-down))
                        (4 `(draw-point obj display :in))
                        (5 `(draw-point obj display :in-down))))
          (t (case byte
               (6 `(save/restore obj))
               (7 `(flip obj)))))))

(defmacro compile-outline (name (&rest encoding))
  `(progn
     (defclass ,name (ship) ())
     (defmethod spacewar:draw ((obj ,name) display)
       ,@(loop for word in encoding
               append (loop with bits = (integer-length word)
                            with rest = (mod bits 3)
                            for pos = (if (> rest 0) (- bits rest) (- bits 3))
                              then (- pos 3)
                            for size = (if (> rest 0) rest 3)
                              then 3
                            for byte = (byte size pos)
                            while (>= pos 0)
                            for instruction = (generate-instruction (ldb byte word))
                            if instruction
                              collect instruction)))))

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

(defmethod spacewar:update ((obj ship))
  (incf (theta obj) 0.01)
  (decf (x obj) (* (sine obj) .5))
  (decf (y obj) (* (cosine obj) .85)))
