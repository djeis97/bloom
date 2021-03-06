(in-package :bloom)

(define-component sprite (:before (render))
  (spritesheet nil)
  (spec nil)
  (name nil)
  (initial-index nil)
  (index nil)
  (frames 1)
  (geometry nil))

(defclass spritesheet ()
  ((%name :reader name
          :initarg :name)
   (%spec :reader spec
          :initarg :spec)
   (%sprites :reader sprites
             :initform (au:dict #'equalp))))

(defmethod write-shader-buffer (buffer (object spritesheet))
  (with-slots (%spec %sprites) object
    (loop :with sprite-count = (length %spec)
          :with xs = (make-array sprite-count)
          :with ys = (make-array sprite-count)
          :with ws = (make-array sprite-count)
          :with hs = (make-array sprite-count)
          :for sprite :in %spec
          :for i :from 0
          :do (destructuring-bind (&key id (x 0) (y 0) (w 0) (h 0)) sprite
                (setf (aref xs i) x
                      (aref ys i) y
                      (aref ws i) w
                      (aref hs i) h
                      (au:href %sprites id) i))
          :finally (shadow:write-buffer-path buffer :x xs)
                   (shadow:write-buffer-path buffer :y ys)
                   (shadow:write-buffer-path buffer :w ws)
                   (shadow:write-buffer-path buffer :h hs))))

(defun make-spritesheet (sprite)
  (with-slots (%core %name %shaders %spec) sprite
    (au:mvlet* ((path (resolve-path :misc %spec))
                (spritesheet (make-instance 'spritesheet
                                            :spec (au:safe-read-file-form path)
                                            :name %spec))
                (vao (gl:gen-vertex-array))
                (blocks binding (make-shader-blocks %core %shaders %name)))
      (make-shader-buffer %name (first blocks) binding sprite)
      (setf (au:href (storage %core) 'spritesheet-geometry) vao)
      spritesheet)))

(defun draw-sprite (sprite)
  (with-slots (%index %geometry) sprite
    (shadow:uniform-int 'bloom.shader:sprite :sprite.index %index)
    (gl:bind-vertex-array %geometry)
    (gl:draw-arrays :points 0 1)
    (gl:bind-vertex-array 0)))

;;; Component event hooks

(defmethod on-component-create ((self sprite))
  (with-slots (%core %spec %spritesheet %geometry %name %initial-index
               %index)
      self
    (setf %spritesheet (cache-lookup %core :spec %spec
                         (make-spritesheet self))
          %geometry (au:href (storage %core) 'spritesheet-geometry)
          %index (au:href (sprites %spritesheet) %name)
          %initial-index %index)))
