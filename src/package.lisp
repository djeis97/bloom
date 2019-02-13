(in-package :defpackage+-user-1)

(defpackage+ #:bloom
  (:local-nicknames (#:m #:game-math))
  (:use #:cl)

  ;; project
  (:export #:define-project)

  ;; engine
  (:export
   #:launch
   #:stop)

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
   #:game-state
   #:get-entity-component
   #:make-shader-blocks
   #:make-shader-buffer
   #:make-entity
   #:make-scene
   #:material
   #:mesh
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
