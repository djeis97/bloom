(in-package :bloom.shader)

(bloom:define-shader-function mesh/v ((pos :vec3)
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

(bloom:define-shader-function colored/f ((uv :vec2)
                                         &uniform
                                         (color :vec3)
                                         (opacity :float))
  (vec4 color opacity))

(bloom:define-shader-function textured/f ((uv :vec2)
                                          &uniform
                                          (sampler :sampler-2d)
                                          (opacity :float))
  (let ((color (.rgb (texture sampler uv))))
    (vec4 color opacity)))

(bloom:define-shader colored (:version 430)
  (:vertex (mesh/v :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (colored/f :vec2)))

(bloom:define-shader textured (:version 430)
  (:vertex (mesh/v :vec3 :vec3 :vec4 :vec4 :vec2 :vec2 :vec4 :vec4))
  (:fragment (textured/f :vec2)))
