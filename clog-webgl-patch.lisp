(in-package :clog-webgl)

(export '(texture-image-2d
          copy-texture-image-2d
          bind-canvas-frame-buffer
          blit-frame-buffer
          read-buffer))

(defgeneric texture-image-2d (clog-webgl glenum-target level glenum-internal-format
                              width height border glenum-format glenum-type source)
  (:documentation "Specifies a two-dimensional texture image.
target:
A GLenum specifying the binding point (target) of the active texture. Possible values:

:TEXTURE_2D
A two-dimensional texture.

:TEXTURE_CUBE_MAP_POSITIVE_X
Positive X face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_NEGATIVE_X
Negative X face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_POSITIVE_Y
Positive Y face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_NEGATIVE_Y
Negative Y face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_POSITIVE_Z
Positive Z face for a cube-mapped texture.

:TEXTURE_CUBE_MAP_NEGATIVE_Z
Negative Z face for a cube-mapped texture.

level:
A GLint specifying the level of detail. Level 0 is the base image
level and level n is the n-th mipmap reduction level.

internal-format:
A GLenum specifying the color components in the texture. Possible values:

:RGB, :RGBA, :LUMINANCE_ALPHA, :LUMINANCE, :ALPHA, :R8, :R8_SNORM, :RG8, :RG8_SNORM, :RGB8,
:RGB8_SNORM, :RGB565, :RGBA4, :RGB5_A1, :RGBA8, :RGBA8_SNORM, :RGB10_A2, :RGB10_A2UI, :SRGB8,
:SRGB8_ALPHA8, :R16F, :RG16F, :RGB16F, :RGBA16F, :R32F, :RG32F, :RGB32F, :RGBA32F, :R11F_G11F_B10F,
:RGB9_E5, :R8I, :R8UI, :R16I, :R16UI, :R32I, :R32UI, :RG8I, :RG8UI, :RG16I, :RG16UI, :RG32I,
:RG32UI, :RGB8I, :RGB8UI, :RGB16I, :RGB16UI, :RGB32I, :RGB32UI, :RGBA8I, :RGBA8UI, :RGBA16I,
:RGBA16UI, :RGBA32I, :RGBA32UI.

width:
A GLsizei specifying the width of the texture.

height:
A GLsizei specifying the height of the texture.

border:
A GLint specifying the width of the border. Must be 0.

format:
A GLenum specifying the format of the texel data. The combinations are listed in this table:
https://www.khronos.org/registry/webgl/specs/latest/2.0/#TEXTURE_TYPES_FORMATS_FROM_DOM_ELEMENTS_TABLE

type:
A GLenum specifying the data type of the texel data. Possible values:

:UNSIGNED_BYTE:
8 bits per channel for :RGBA

:UNSIGNED_SHORT_5_6_5:
5 red bits, 6 green bits, 5 blue bits.

:UNSIGNED_SHORT_4_4_4_4:
4 red bits, 4 green bits, 4 blue bits, 4 alpha bits.

:UNSIGNED_SHORT_5_5_5_1:
5 red bits, 5 green bits, 5 blue bits, 1 alpha bit.

:BYTE

:UNSIGNED_SHORT

:SHORT

:UNSIGNED_INT

:INT

:HALF_FLOAT

:FLOAT

:UNSIGNED_INT_2_10_10_10_REV

:UNSIGNED_INT_10F_11F_11F_REV

:UNSIGNED_INT_5_9_9_9_REV

:UNSIGNED_INT_24_8

:FLOAT_32_UNSIGNED_INT_24_8_REV (pixels must be null)

source:
Can be NIL, a CLOG-IMG object or a CLOG-IMAGE-DATA object."))

(defmethod texture-image-2d ((webgl clog-webgl) glenum-target level glenum-internal-format
                             width height border glenum-format glenum-type source)
  (execute webgl (format nil
                         "texImage2D(~A.~A, ~D, ~A.~A, ~D, ~D, ~D, ~A.~A, ~A.~A~:[, null~;, ~:*~A~])"
                         (script-id webgl) glenum-target level
                         (script-id webgl) glenum-internal-format
                         width height border
                         (script-id webgl) glenum-format
                         (script-id webgl) glenum-type
                         (if source (script-id source) nil))))

(defgeneric copy-texture-image-2d (webgl glenum-target level glenum-internal-format
                                   x y width height border)
  (:documentation "Copies pixels from the current WebGLFramebuffer into a 2D
texture image."))

(defmethod copy-texture-image-2d ((webgl clog-webgl) glenum-target level
                                  glenum-internal-format x y width height border)
  (execute webgl
           (format nil
                   "copyTexImage2D(~A.~A, ~D, ~A.~A, ~D, ~D, ~D, ~D, ~D)"
                   (script-id webgl) glenum-target level
                   (script-id webgl) glenum-internal-format
                   x y width height border)))

(defgeneric read-buffer (webgl glenum-source)
  (:documentation "Selects a color buffer as the source for pixels for subsequent calls to copy-tex-image-2d, copy-tex-sub-image-2d, copy-tex-sub-image-3d or read-pixels."))

(defmethod read-buffer ((webgl clog-webgl) glenum-source)
  (execute webgl (format nil "readBuffer(~A.~A)" (script-id webgl) glenum-source)))

(defgeneric bind-canvas-frame-buffer (webgl glenum-target)
  (:documentation "Binds to the specified target the provided WebGLFramebuffer, or, if the framebuffer argument is null, the default WebGLFramebuffer, which is associated with the canvas rendering context."))

(defmethod bind-canvas-frame-buffer ((webgl clog-webgl) glenum-target)
  (execute webgl (format nil "bindFramebuffer(~A.~A, null)" (script-id webgl) glenum-target)))

(defgeneric blit-frame-buffer (webgl src-x0 src-y0 src-x1 src-y1 dst-x0 dst-y0 dst-x1 dst-y1 glenum-mask glenum-filter)
  (:documentation "Transfers a block of pixels from the read framebuffer to the draw framebuffer. Read and draw framebuffers are bound using bind-frame-buffer()."))

(defmethod blit-frame-buffer ((webgl clog-webgl) src-x0 src-y0 src-x1 src-y1 dst-x0 dst-y0 dst-x1 dst-y1 glenum-mask glenum-filter)
  (execute webgl (format nil "blitFramebuffer(~D, ~D, ~D, ~D, ~D, ~D, ~D, ~D, ~A.~A, ~A.~A)" src-x0 src-y0 src-x1 src-y1 dst-x0 dst-y0 dst-x1 dst-y1 (script-id webgl) glenum-mask (script-id webgl) glenum-filter)))

(defgeneric create-webgl (clog-canvas &key context attributes)
  (:documentation "Create a new CLOG-WebGL from a CLOG-Canvas. Context
can be webgl (version 1) or webgl2 (default). Attributes must be a
plist like (\"attribute\" value ...). The values can be booleans or
strings."))

(defmethod create-webgl ((obj clog-canvas) &key (context "webgl2") attributes)
  (let ((web-id (generate-id)))
    (js-execute obj (format nil "clog['~A']=clog['~A'].getContext('~A'~@[,{~{~A: ~A~^, ~}}~])"
                            web-id (html-id obj) context
                            (loop for (key value) on attributes by #'cddr
                                  append (list key (if (typep value 'boolean)
                                                       (if value "true" "false")
                                                       (format nil "\"~A\"" value))))))
    (make-instance 'clog-webgl
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))
