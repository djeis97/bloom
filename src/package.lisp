(in-package :defpackage+-user-1)

(defpackage+ #:bloom
  (:local-nicknames (#:m #:game-math))
  (:use #:cl)

  ;; core
  (:export
   #:launch
   #:stop)

  ;; project
  (:export
   #:define-project)

  ;; transforms
  (:export
   #:translate-transform
   #:rotate-transform
   #:scale-transform
   #:get-translation
   #:get-rotation
   #:get-scale0)

  ;; actions
  (:export
   #:action
   #:make-action
   #:find-action
   #:on-action-insert
   #:on-action-update
   #:on-action-finish
   #:action-step
   #:action/fade
   #:action/sprite-animate
   #:action/translate
   #:action/rotate
   #:action/move-tile)

  ;; prefabs
  (:export
   #:define-prefab
   #:print-prefab)

  ;; components
  (:export
   #:on-component-attach
   #:on-component-create
   #:on-component-destroy
   #:on-component-detach
   #:on-component-physics-update
   #:on-component-update
   #:on-component-render)

  ;; input
  (:export
   #:input-enter-p
   #:input-enabled-p
   #:input-exit-p)

  (:export
   #:cache-lookup
   #:camera
   #:define-component
   #:define-framebuffer
   #:define-material
   #:define-resource-paths
   #:define-scenes
   #:define-texture
   #:define-texture-profile
   #:entity
   #:core
   #:get-entity-component
   #:make-shader-blocks
   #:make-shader-buffer
   #:make-entity
   #:make-scene
   #:material
   #:mesh
   #:render
   #:shader-modified-post-hook
   #:sprite
   #:storage
   #:switch-camera-target
   #:switch-scene
   #:tag
   #:transform
   #:uniforms
   #:value
   #:write-shader-buffer))

(defpackage+ #:bloom.shader
  (:local-nicknames (#:b #:bloom))
  (:use #:cl #:umbra.swizzle)
  (:inherit #:umbra))
