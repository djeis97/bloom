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
           #:define-prototype
           #:define-resource-paths
           #:define-scene
           #:define-shader
           #:define-shader-function
           #:define-shader-struct
           #:define-texture
           #:define-texture-profile
           #:game-state
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
           #:on-component-delete
           #:on-component-detach
           #:on-component-update
           #:on-component-render
           #:render
           #:renderer
           #:rotate
           #:rotate-transform
           #:shader-modified-post-hook
           #:shared-storage
           #:sprite
           #:sprite-animate
           #:start-engine
           #:stop-engine
           #:switch-scene
           #:tag
           #:transform
           #:uniforms
           #:value
           #:world
           #:world/cell-count
           #:world/cell-index
           #:write-shader-buffer))

(defpackage+ #:bloom.shader
  (:local-nicknames (#:b #:bloom))
  (:use #:cl)
  (:inherit #:shadow.vari))
