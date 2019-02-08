(in-package :bloom)

(define-component camera (:after (transform))
  (scene nil)
  (active-p nil)
  (view (m:mat4 1))
  (projection (m:mat4 1))
  (mode :orthographic)
  (clip-near 0.1)
  (clip-far 1024)
  (fov-y 45.0)
  (zoom 1)
  (transform nil)
  (target nil)
  (follow-z-axis-p nil))

(defun set-camera-view (camera)
  (with-slots (%target %follow-z-axis-p) camera
    (let* ((target (m:get-translation
                    (model (get-entity-component (target camera) 'transform))))
           (camera-model (model (transform camera)))
           (eye (if %target
                    (m:+ (m:get-translation camera-model)
                         (if %follow-z-axis-p
                             target
                             (m:vec3 (m:vec2 target))))
                    (m:get-translation camera-model)))
           (target (m:+ eye (m:negate (m:vec3 (m:get-column camera-model 2)))))
           (up (m:vec3 (m:get-column camera-model 1))))
      (m:set-view eye target up (view camera)))))

(defmethod set-camera-projection ((mode (eql :orthographic)) camera game-state)
  (with-slots (%clip-near %clip-far %zoom %projection) camera
    (let ((w (/ (option game-state :window-width) %zoom 2))
          (h (/ (option game-state :window-height) %zoom 2)))
      (m:set-projection/orthographic
       (- w) w (- h) h %clip-near %clip-far %projection))))

(defmethod set-camera-projection ((mode (eql :perspective)) camera game-state)
  (with-slots (%fov-y %zoom %clip-near %clip-far %projection) camera
    (let ((aspect-ratio (/ (option game-state :window-width)
                           (option game-state :window-height))))
      (m:set-projection/perspective (/ %fov-y %zoom)
                                    aspect-ratio
                                    %clip-near
                                    %clip-far
                                    %projection))))

(defmethod set-camera-projection ((mode (eql :isometric)) camera game-state)
  (with-slots (%rotation) (transform camera)
    (let ((rotation (m:vec3 (- (asin (/ (sqrt 3)))) 0 (/ pi 4))))
      (set-camera-projection :orthographic camera game-state)
      (setf (current %rotation) (m:inverse
                                 (m:rotate :local m:+id-quat+ rotation))))))

(defun zoom-camera (game-state offset)
  (let ((camera (camera (active-scene game-state))))
    (setf (fov-y camera) (au:clamp (- (fov-y camera) offset) 1.0 45.0))
    (set-camera-projection (mode camera) camera game-state)))

(defun correct-camera-transform (camera)
  (with-slots (%actor %mode %transform) camera
    (when (m:zero-p (current (translation %transform)))
      (let ((translation (ecase %mode
                           ((:orthographic :isometric) (m:vec3 0 0 1))
                           (:perspective (m:vec3 0 0 50)))))
        (translate-transform %transform translation)))))

(defun switch-camera-target (target)
  (let ((camera (camera (active-scene (game-state target)))))
    (setf (target camera) target)))

;;; Component event hooks

(defmethod on-component-attach ((self camera))
  (with-slots (%game-state %entity %scene %mode %fov-y %transform) self
    (setf %scene (active-scene %game-state)
          (camera %scene) self
          %fov-y (* %fov-y (/ pi 180))
          %transform (get-entity-component %entity 'transform))
    (correct-camera-transform self)
    (set-camera-projection %mode self %game-state)))

(defmethod on-component-update ((self camera))
  (set-camera-view self))
