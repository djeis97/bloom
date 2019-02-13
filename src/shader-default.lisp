(in-package :bloom.shader)

(define-function mesh/v ((mesh-attrs mesh-attrs)
                         &uniform
                         (model :mat4)
                         (view :mat4)
                         (proj :mat4))
  (with-slots (mesh/pos mesh/uv1) mesh-attrs
    (values (* proj view model (vec4 mesh/pos 1))
            mesh/uv1)))

(define-function colored/f ((uv :vec2)
                            &uniform
                            (color :vec3)
                            (opacity :float))
  (vec4 color opacity))

(define-function textured/f ((uv :vec2)
                             &uniform
                             (sampler :sampler-2d)
                             (opacity :float))
  (let ((color (.rgb (texture sampler uv))))
    (vec4 color opacity)))

(define-shader colored ()
  (:vertex (mesh/v mesh-attrs))
  (:fragment (colored/f :vec2)))

(define-shader textured ()
  (:vertex (mesh/v mesh-attrs))
  (:fragment (textured/f :vec2)))
