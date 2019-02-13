(asdf:defsystem #:bloom
  :description "A powerful framework for creating 2D and 3D video games."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/bloom"
  :source-control (:git "https://github.com/mfiano/bloom.git")
  :bug-tracker "https://github.com/mfiano/bloom/issues"
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:closer-mop
               #:defpackage-plus
               #:queues.simple-cqueue
               #:sdl2
               #:sdl2-image
               #:jsown
               #:parsley
               #:cl-graph
               #:verbose
               #:golden-utils
               #:game-math
               #:doubly-linked-list
               #:shadow
               #:umbra)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "resource")
   (:file "project")
   (:file "logging")
   (:file "live-coding")
   (:file "engine")
   (:file "tasks")
   (:file "resource-cache")
   (:file "host")
   (:file "input-states")
   (:file "input-keyboard")
   (:file "input-mouse")
   (:file "input-gamepad")
   (:file "input-window")
   (:file "input-handler")
   (:file "frame")
   (:file "asset-image")
   (:file "asset-mesh")
   (:file "texture")
   (:file "material")
   (:file "framebuffer")
   (:file "flow")
   (:file "entity")
   (:file "scene")
   (:file "display")
   (:file "prefab-checks")
   (:file "prefab")
   (:file "shader")
   (:file "shader-default")
   (:file "shader-sprite")
   (:file "component")
   (:file "component-tag")
   (:file "component-group")
   (:file "component-transform")
   (:file "component-camera")
   (:file "component-mesh")
   (:file "component-sprite")
   (:file "component-render")
   (:file "action")
   (:file "action-translate")
   (:file "action-rotate")
   (:file "action-fade")
   (:file "action-sprite-animate")))
