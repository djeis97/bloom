(in-package :defpackage+-user-1)

(defpackage+ #:bloom
  (:local-nicknames (#:m #:game-math))
  (:use #:cl)

  ;; engine
  (:export #:launch
           #:stop)

  ;; actions
  (:export #:action
           #:make-action
           #:on-action-insert
           #:on-action-update
           #:on-action-finish
           #:action-step
           #:action/fade
           #:action/sprite-animate
           #:action/translate
           #:action/rotate)

  (:export #:cache-lookup
           #:camera
           #:define-asset-pool
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
           #:get-entity-component
           #:input-enter-p
           #:input-enabled-p
           #:input-exit-p
           #:make-shader-blocks
           #:make-shader-buffer
           #:make-entity
           #:make-scene
           #:material
           #:mesh
           #:on-component-attach
           #:on-component-create
           #:on-component-destroy
           #:on-component-detach
           #:on-component-update
           #:on-component-render
           #:print-prefab
           #:render
           #:rotate-transform
           #:scale-transform
           #:shader-modified-post-hook
           #:sprite
           #:storage
           #:switch-camera-target
           #:switch-scene
           #:tag
           #:transform
           #:translate-transform
           #:uniforms
           #:value
           #:write-shader-buffer))

(defpackage+ #:bloom.shader
  (:local-nicknames (#:b #:bloom))
  (:use #:cl #:umbra.swizzle)
  (:inherit #:umbra))
