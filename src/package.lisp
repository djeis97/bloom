(in-package :defpackage+-user-1)

(defpackage+ #:bloom
  (:local-nicknames (#:m #:game-math))
  (:use #:cl)
  (:export #:action
           #:action-step
           #:attrs
           #:cache-lookup
           #:camera
           #:cycle-p
           #:define-component
           #:define-framebuffer
           #:define-material
           #:define-options
           #:define-prefab
           #:define-resource-paths
           #:define-scenes
           #:define-shader
           #:define-shader-function
           #:define-shader-struct
           #:define-texture
           #:define-texture-profile
           #:entity
           #:game-state
           #:get-entity-component-by-type
           #:input-enter-p
           #:input-enabled-p
           #:input-exit-p
           #:make-action
           #:make-action-from-existing
           #:make-shader-blocks
           #:make-shader-buffer
           #:make-entity
           #:make-scene
           #:material
           #:mesh
           #:on-action-insert
           #:on-action-finish
           #:on-action-update
           #:on-component-attach
           #:on-component-create
           #:on-component-detach
           #:on-component-update
           #:on-component-render
           #:print-prefab
           #:render
           #:rotate
           #:rotate-transform
           #:scale-transform
           #:shader-modified-post-hook
           #:sprite
           #:sprite-animate
           #:start-engine
           #:stop-engine
           #:storage
           #:switch-scene
           #:tag
           #:transform
           #:translate
           #:translate-transform
           #:uniforms
           #:value
           #:write-shader-buffer))

(defpackage+ #:bloom.shader
  (:local-nicknames (#:b #:bloom))
  (:use #:cl #:umbra.swizzle)
  (:inherit #:umbra))
