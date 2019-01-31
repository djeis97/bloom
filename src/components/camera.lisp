(in-package :bloom)

(define-component camera (:after (transform))
  (scene nil)
  (active-p nil)
  (view (m:mat4 1))
  (projection (m:mat4 1))
  (mode :orthographic)
  (clip-near -10000.0)
  (clip-far 10000.0)
  (fov-y 45.0)
  (transform nil))

(defun set-camera-view (camera)
  (let* ((model (model (transform camera)))
         (eye (m:get-translation model))
         (target (m:+ eye (m:negate (m:vec3 (m:get-column model 2)))))
         (up (m:vec3 (m:get-column model 1))))
    (m:set-view eye target up (view camera))))

(defmethod set-camera-projection ((mode (eql :orthographic)) camera game-state)
  (let ((w (/ (option game-state :window-width) 2))
        (h (/ (option game-state :window-height) 2)))
    (m:set-projection/orthographic (- w)
                                   w
                                   (- h)
                                   h
                                   (clip-near camera)
                                   (clip-far camera)
                                   (projection camera))))

(defmethod set-camera-projection ((mode (eql :perspective)) camera game-state)
  (let ((aspect-ratio (/ (option game-state :window-width)
                         (option game-state :window-height))))
    (m:set-projection/perspective (fov-y camera)
                                  aspect-ratio
                                  (clip-near camera)
                                  (clip-far camera)
                                  (projection camera))))

(defmethod set-camera-projection ((mode (eql :isometric)) camera game-state)
  (with-slots (%rotation) (transform camera)
    (let ((rotation (m:vec3 (- (asin (/ (sqrt 3)))) 0 (/ pi 4))))
      (set-camera-projection :orthographic camera game-state)
      (setf (current %rotation) (m:inverse (m:rotate :local m:+id-quat+ rotation))))))

(defun zoom-camera (game-state offset)
  (let ((camera (camera (active-scene game-state))))
    (setf (fov-y camera) (au:clamp (- (fov-y camera) offset) 1.0 45.0))
    (set-camera-projection (mode camera) camera game-state)))

;;; Component event hooks

(defmethod on-component-attach ((component camera))
  (setf (scene component) (active-scene (game-state component))
        (camera (scene component)) component
        (fov-y component) (* (fov-y component) (/ pi 180))
        (transform component) (get-entity-component-by-type (entity component) 'transform))
  (set-camera-projection (mode component) component (game-state component)))

(defmethod on-component-update ((component camera))
  (set-camera-view component))
