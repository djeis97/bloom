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
  :depends-on (#:split-sequence
               #:defpackage-plus
               #:queues.simple-cqueue
               #:local-time
               #:sdl2
               #:cl-tga
               #:pngload
               #:jsown
               #:parsley
               #:cl-graph
               #:verbose
               #:golden-utils
               #:game-math
               #:doubly-linked-list
               #:shadow
               #:dungen)
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
     (:file "engine")
     (:file "resource-cache")
     (:file "host")
     (:file "display")
     (:file "frame")
     (:file "texture")
     (:file "material")
     (:file "shader")
     (:file "framebuffer")
     (:file "flow")
     (:file "component")
     (:file "prototype")
     (:file "entity")
     (:file "scene")))
   (:module "components"
    :components
    ((:file "tag")
     (:file "group")
     (:file "transform")
     (:file "camera")
     (:file "mesh")
     (:file "action")
     (:file "sprite")
     (:file "render")))
   (:module "shaders"
    :components
    ((:file "default")
     (:file "sprite")))
   (:module "addons"
    :components
    ((:file "actions")))))
