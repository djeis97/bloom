(in-package :bloom.shader)

(b:define-shader-struct sprite-data ()
  (sampler :sampler-2d :accessor sampler)
  (index :int :accessor index))

(b:define-shader-struct spritesheet-data ()
  (x (:float 2048) :accessor x)
  (y (:float 2048) :accessor y)
  (w (:float 2048) :accessor w)
  (h (:float 2048) :accessor h))

(b:define-shader-function sprite/v ()
  (values))

(b:define-shader-function sprite/g (&uniform
                                    (model :mat4)
                                    (view :mat4)
                                    (proj :mat4)
                                    (sprite sprite-data)
                                    (spritesheet spritesheet-data :ssbo :std-430))
  (declare (output-primitive :kind :triangle-strip :max-vertices 6))
  (let* ((mvp (* proj view model))
         (extents (vec4 (aref (x spritesheet) (index sprite))
                        (aref (y spritesheet) (index sprite))
                        (aref (w spritesheet) (index sprite))
                        (aref (h spritesheet) (index sprite))))
         (size (.xyxy (texture-size (sampler sprite) 0)))
         (offsets (* size (vec4 (* 0.5 (.zw extents)) (* -0.5 (.zw extents))))))
    (setf (.zw extents) (+ (.xy extents) (.zw extents)))
    (emit ()
          (* mvp (vec4 (.xy offsets) 0 1))
          (.xw extents))
    (emit ()
          (* mvp (vec4 (.xw offsets) 0 1))
          (.xy extents))
    (emit ()
          (* mvp (vec4 (.zy offsets) 0 1))
          (.zw extents))
    (emit ()
          (* mvp (vec4 (.zw offsets) 0 1))
          (.zy extents))
    (end-primitive))
  (values))

(b:define-shader-function sprite/f ((uv :vec2)
                                    &uniform
                                    (sprite sprite-data))
  (let ((color (texture (sampler sprite) uv)))
    (if (zerop (.a color))
        (discard)
        color)))

(b:define-shader sprite (:primitive :points)
  (:vertex (sprite/v))
  (:geometry (sprite/g))
  (:fragment (sprite/f :vec2)))
