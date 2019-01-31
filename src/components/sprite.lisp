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
          :with xs = (make-array sprite-count :element-type 'single-float)
          :with ys = (make-array sprite-count :element-type 'single-float)
          :with ws = (make-array sprite-count :element-type 'single-float)
          :with hs = (make-array sprite-count :element-type 'single-float)
          :for sprite :in %spec
          :for i :from 0
          :do (destructuring-bind (&key id (x 0f0) (y 0f0) (w 0f0) (h 0f0)) sprite
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
  (with-slots (%game-state %name %shaders %spec) sprite
    (au:mvlet* ((path (resolve-path :misc %spec))
                (spritesheet (make-instance 'spritesheet
                                            :spec (au:safe-read-file-form path) :name %spec))
                (vao (gl:gen-vertex-array))
                (blocks binding (make-shader-blocks %game-state %shaders %name)))
      (make-shader-buffer %name (first blocks) binding sprite)
      (setf (au:href (shared-storage %game-state) :spritesheet-geometry) vao)
      spritesheet)))

(defun draw-sprite (sprite)
  (with-slots (%index %geometry) sprite
    (shadow:uniform-int 'bloom.shader:sprite :sprite.index %index)
    (gl:bind-vertex-array %geometry)
    (gl:draw-arrays :points 0 1)
    (gl:bind-vertex-array 0)))

;;; Component event hooks

(defmethod on-component-create ((component sprite))
  (with-slots (%game-state %spec %spritesheet %geometry %name %initial-index %index) component
    (setf %spritesheet (cache-lookup %game-state :spec %spec
                         (make-spritesheet component))
          %geometry (au:href (shared-storage %game-state) :spritesheet-geometry)
          %index (au:href (sprites %spritesheet) %name)
          %initial-index %index)))
