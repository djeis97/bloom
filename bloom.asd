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
               #:local-time
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
   (:module "utility"
    :components
    ((:file "common")
     (:file "live-coding")))
   (:module "asset"
    :components
    ((:file "image")
     (:file "mesh")
     (:file "resource")))
   (:module "input"
    :components
    ((:file "states")
     (:file "keyboard")
     (:file "mouse")
     (:file "gamepad")
     (:file "window")
     (:file "handler")))
   (:module "core"
    :components
    ((:file "options")
     (:file "logging")
     (:file "tasks")
     (:file "resource-cache")
     (:file "host")
     (:file "frame")
     (:file "texture")
     (:file "material")
     (:file "shader")
     (:file "framebuffer")
     (:file "flow")
     (:file "component")
     (:file "entity")
     (:file "action")
     (:file "scene")
     (:file "engine")
     (:file "display")))
   (:module "shaders"
    :components
    ((:file "default")
     (:file "sprite")))
   (:module "components"
    :components
    ((:file "tag")
     (:file "group")
     (:file "transform")
     (:file "camera")
     (:file "mesh")
     (:file "sprite")
     (:file "render")))
   (:module "actions"
    :components
    ((:file "translate")
     (:file "rotate")
     (:file "fade")
     (:file "sprite-animate")))
   (:module "prefab"
    :components
    ((:file "checks")
     (:file "prefab")))))
