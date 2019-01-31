(in-package :bloom.shader)

(b:define-shader-function mesh/v ((pos :vec3)
                                  (normal :vec3)
                                  (tangent :vec4)
                                  (color :vec4)
                                  (uv1 :vec2)
                                  (uv2 :vec2)
                                  (joints :vec4)
                                  (weights :vec4)
                                  &uniform
                                  (model :mat4)
                                  (view :mat4)
                                  (proj :mat4))
  (values (* proj view model (vec4 pos 1))
          uv1))

(b:define-shader-function colored/f ((uv :vec2)
                                     &uniform
                                     (color :vec3)
                                     (opacity :float))
  (vec4 color opacity))

(b:define-shader-function textured/f ((uv :vec2)
                                      &uniform
                                      (sampler :sampler-2d)
                                      (opacity :float))
  (let ((color (.rgb (texture sampler uv))))
    (vec4 color opacity)))

(b:define-shader colored ()
  (:vertex (mesh/v :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (colored/f :vec2)))

(b:define-shader textured ()
  (:vertex (mesh/v :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (textured/f :vec2)))
