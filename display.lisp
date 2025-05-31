(in-package #:display)

(defclass gldata ()
  ((webgl :initarg :webgl :reader webgl)
   (vbo :reader vbo)
   (vao :reader vao)
   (program :initarg :program :accessor program)))

(defmethod initialize-instance :after ((instance gldata) &rest initargs
                                       &key &allow-other-keys)
  (with-slots (vbo vao) instance
    (let ((webgl (getf initargs :webgl)))
      (setf vao (gl:create-vertex-array webgl)
            vbo (gl:create-webgl-buffer webgl)))))

(defclass texture (gldata)
  ((frame-buffer :reader frame-buffer)
   (buffer :reader buffer)
   (points :reader points)))

(defmethod initialize-instance :after ((instance texture) &rest initargs
                                       &key &allow-other-keys)
  (with-slots (frame-buffer frame-prevbuffer buffer prevbuffer points)
      instance
    (let ((webgl (getf initargs :webgl)))
      (setf frame-buffer (gl:create-webgl-frame-buffer webgl)
            buffer (gl:create-webgl-texture webgl)
            points (loop repeat 4
                         collect (make-array 1024
                                             :element-type 'single-float
                                             :initial-element 0.0f0
                                             :adjustable t
                                             :fill-pointer 0))))))

(defclass quad (gldata)
  ((ebo :reader ebo)
   (texture-buffer :initarg :texture-buffer :reader texture-buffer)))

(defmethod initialize-instance :after ((instance quad) &rest initargs &key &allow-other-keys)
  (with-slots (vao ebo vbo) instance
    (gl:bind-vertex-array vao)
    (let* ((webgl (getf initargs :webgl))
           (program (getf initargs :program))
           (pos (gl:attribute-location program "aPos"))
           (tex-coords (gl:attribute-location program "aTexCoords"))
           (screen-tex (gl:uniform-location program "screenTexture")))
      (setf ebo (gl:create-webgl-buffer webgl))
      (gl:bind-buffer vbo :ARRAY_BUFFER)
      (gl:bind-buffer ebo :ELEMENT_ARRAY_BUFFER)
      (gl:buffer-data vbo (coerce *quad* 'list) "Float32Array" :STATIC_DRAW)
      (gl:buffer-data ebo (coerce *quad-elems* 'list) "Uint16Array" :STATIC_DRAW)
      (gl:vertex-attribute-pointer webgl pos 2 :FLOAT nil 16 0)
      (gl:vertex-attribute-pointer webgl tex-coords 2 :FLOAT nil 16 8)
      (gl:enable-vertex-attribute-array webgl pos)
      (gl:enable-vertex-attribute-array webgl tex-coords)
      (gl:uniform-integer webgl screen-tex 0))))

(defparameter *v-shader* "#version 300 es
in vec2 posicion;
out vec3 Color;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform vec3 color;
uniform float size;

void main() {
  gl_PointSize=size;
  Color = color;
  gl_Position = proj*view*model*vec4(posicion, 0.0, 1.0);
}")

(defparameter *f-shader* "#version 300 es
precision highp float;
in vec3 Color;
out vec4 outColor;

void main() {
  vec2 coord = 2.0 * gl_PointCoord - 1.0;
  if ( dot(coord, coord) > 1.0 )
    discard;
  outColor = vec4(Color, 1.0);
}")

(defparameter *quad*
  (make-array 16 :element-type 'single-float
                 :initial-contents '(-1.0  1.0 0.0 1.0
                                     1.0  1.0 1.0 1.0
                                     1.0 -1.0 1.0 0.0
                                     -1.0 -1.0 0.0 0.0)))

(defparameter *quad-elems*
  (make-array 6 :element-type 'fixnum
                :initial-contents '(0 1 2
                                    2 3 0)))

(defparameter *quad-v-shader*
  "#version 300 es
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoords;

out vec2 TexCoords;

void main()
{
    gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);
    TexCoords = aTexCoords;
}")

(defparameter *quad-f-shader*
  "#version 300 es
precision highp float;
out vec4 FragColor;

in vec2 TexCoords;

uniform sampler2D screenTexture;

void main()
{
    FragColor = texture(screenTexture, TexCoords);
}")


(defun compile-program (webgl vertex-shader fragment-shader)
  (let ((program (gl:compile-webgl-program webgl
                                           (gl:compile-shader-source webgl :VERTEX_SHADER
                                                                     vertex-shader)
                                           (gl:compile-shader-source webgl :FRAGMENT_SHADER
                                                                     fragment-shader))))
    (gl:use-program program)
    program))

(defun make-texture (webgl width height &key (v-shader *v-shader*) (f-shader *f-shader*))
  (let* ((med-width (/ width 2f0))
         (med-height (/ height 2f0))
         (program (compile-program webgl v-shader f-shader))
         (obj (make-instance 'texture
                             :webgl webgl
                             :program program)))
    (gl:uniform-matrix webgl 4 (gl:uniform-location program "view") nil
                       (coerce (rtg-math.matrix4:identity) 'list))
    (gl:uniform-matrix webgl 4 (gl:uniform-location program "proj") nil
                       (coerce (kit.math:ortho-matrix
                                (- med-width) med-width
                                (- med-height) med-height
                                0.0001 3000.0)
                               'list))
    (gl:uniform-matrix webgl 4 (gl:uniform-location program "model") nil
                       (coerce (rtg-math.matrix4:identity) 'list))
    (gl:bind-vertex-array (vao obj))
    (gl:bind-buffer (vbo obj) :ARRAY_BUFFER)
    (gl:enable-vertex-attribute-array webgl (gl:attribute-location program "posicion"))
    (gl:vertex-attribute-pointer webgl (gl:attribute-location program "posicion") 2 :FLOAT nil 8 0)
    ;;
    (gl:bind-frame-buffer (frame-buffer obj) :DRAW_FRAMEBUFFER)
    (gl:bind-texture (buffer obj) :TEXTURE_2D)
    (gl:texture-image-2d webgl :TEXTURE_2D 0 :RGBA width height 0 :RGBA :UNSIGNED_BYTE nil)
    (gl:texture-parameter-integer webgl :TEXTURE_2D :TEXTURE_MIN_FILTER :LINEAR)
    (gl:texture-parameter-integer webgl :TEXTURE_2D :TEXTURE_MAG_FILTER :LINEAR)
    (gl:frame-buffer-texture-2d webgl :DRAW_FRAMEBUFFER :COLOR_ATTACHMENT0 :TEXTURE_2D
                                (buffer obj) 0)
    obj))

(defun make-quad (webgl texture-buffer &key (v-shader *quad-v-shader*)
                                         (f-shader *quad-f-shader*))
  (make-instance 'quad
                 :webgl webgl
                 :texture-buffer texture-buffer
                 :program (compile-program webgl v-shader f-shader)))

(defgeneric draw (obj)
  (:documentation "Draws OBJ"))

(defmethod draw ((obj texture))
  (gl:use-program (program obj))
  (gl:bind-vertex-array (vao obj))
  (gl:bind-frame-buffer (frame-buffer obj) :DRAW_FRAMEBUFFER)
  (gl:clear-webgl (webgl obj) :COLOR_BUFFER_BIT)
  (gl:bind-buffer (vbo obj) :ARRAY_BUFFER)
  (loop for points in (points obj)
        for intensity from 2.5 by 1.25
        if (plusp (fill-pointer points)) do
          (gl:buffer-data (vbo obj) (coerce points 'list) "Float32Array" :STATIC_DRAW)
          (gl:uniform-float (webgl obj) (gl:uniform-location (program obj) "color")
                            0.1 0.8 0.8)
          (gl:uniform-float (webgl obj) (gl:uniform-location (program obj) "size") intensity)
          (gl:draw-arrays (webgl obj) :POINTS 0 (/ (fill-pointer points) 2))))

(defmethod draw ((obj quad))
  (gl:use-program (program obj))
  (gl:bind-canvas-frame-buffer (webgl obj) :DRAW_FRAMEBUFFER)
  (gl:bind-vertex-array (vao obj))
  (gl:active-texture (webgl obj) :TEXTURE0)
  (gl:bind-texture (texture-buffer obj) :TEXTURE_2D)
  (gl:draw-elements (webgl obj) :TRIANGLES 6 :UNSIGNED_SHORT 0))

;;;; Display
(defclass display ()
  ((texture :initarg :texture :reader texture)
   (quad :initarg :quad :reader quad)
   (width :initarg :width :reader width)
   (width/2 :reader width/2)
   (height :initarg :height :reader height)
   (height/2 :reader height/2)))

(defmethod initialize-instance :after ((instance display) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (width/2 width height/2 height) instance
    (setf width/2 (/ width 2)
          height/2 (/ height 2))))

(defparameter *width* 1024)
(defparameter *height* 1024)

(defun make-display (webgl &optional (width *width*) (height *height*))
  (let ((texture (make-texture webgl width height)))
    (make-instance 'display
                   :width width
                   :height height
                   :texture texture
                   :quad (make-quad webgl (buffer texture)))))

(defun toroid (value display)
  (float (if (< (- (width/2 display)) value (width/2 display))
             value
             (if (minusp value)
                 (+ value (width display))
                 (- value (width display))))
         1.0f0))

(defgeneric draw-point (display x y &optional intensity)
  (:documentation "Draws a point in the display")
  (:method ((display display) x y &optional (intensity 0))
    (with-slots (points) (texture display)
      (let ((points (elt points intensity)))
        (vector-push-extend (toroid x display) points)
        (vector-push-extend (toroid y display) points)))))

(defgeneric draw-points (display new-points &optional intensity)
  (:documentation "Draws multiple points in the display")
  (:method ((display display) new-points &optional (intensity 0))
    (with-slots (points) (texture display)
      (loop with points = (elt points intensity)
            for (x y) on new-points by #'cddr
            do (vector-push-extend (toroid x display) points)
               (vector-push-extend (toroid y display) points)))))

(defmethod draw ((display display))
  (draw (texture display))
  (draw (quad display)))

(defgeneric clear (display)
  (:documentation "Clears the display")
  (:method ((obj display))
    (loop for points in (points (texture obj)) do
      (setf (fill-pointer points) 0))))
