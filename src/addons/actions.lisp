(in-package :bloom)

;;; fade

(defmethod on-action-update (action (name (eql 'fade-in)))
  (setf (value (au:href (uniforms (material (render action))) :opacity))
        (action-step action)))

(defmethod on-action-finish (action (name (eql 'fade-in)))
  (when (cycle-p action)
    (replace-action action 'fade-out)))

(defmethod on-action-update (action (name (eql 'fade-out)))
  (setf (value (au:href (uniforms (material (render action))) :opacity))
        (- 1 (action-step action))))

(defmethod on-action-finish (action (name (eql 'fade-out)))
  (when (cycle-p action)
    (replace-action action 'fade-in)))

;;; rotate

(defmethod on-action-update (action (name (eql 'rotate)))
  (let* ((transform (transform (render action)))
         (attrs (attrs action))
         (angle (or (au:href attrs :angle) (* pi 2)))
         (step (au:map-domain 0 1 0 angle (action-step action))))
    (ecase (or (au:href attrs :axis) :z)
      (:x (rotate-transform transform (m:vec3 step 0 0) :replace-p t))
      (:y (rotate-transform transform (m:vec3 0 step 0) :replace-p t))
      (:z (rotate-transform transform (m:vec3 0 0 step) :replace-p t)))))

(defmethod on-action-finish (action (name (eql 'rotate)))
  (when (cycle-p action)
    (replace-action action 'rotate/reverse)))

(defmethod on-action-update (action (name (eql 'rotate/reverse)))
  (let* ((transform (transform (render action)))
         (attrs (attrs action))
         (angle (or (au:href attrs :angle) (* pi 2)))
         (step (- angle (au:map-domain 0 1 0 angle (action-step action)))))
    (ecase (or (au:href attrs :axis) :z)
      (:x (rotate-transform transform (m:vec3 step 0 0) :replace-p t))
      (:y (rotate-transform transform (m:vec3 0 step 0) :replace-p t))
      (:z (rotate-transform transform (m:vec3 0 0 step) :replace-p t)))))

(defmethod on-action-finish (action (name (eql 'rotate/reverse)))
  (when (cycle-p action)
    (replace-action action 'rotate)))

;;; sprite-animate

(defmethod on-action-update (action (name (eql 'sprite-animate)))
  (au:when-let ((sprite (get-entity-component-by-type (entity action) 'sprite))
                (step (action-step action)))
    (with-slots (%initial-index %index %frames) sprite
      (setf %index (floor
                    (au:map-domain 0 1 %initial-index (1- (+ %initial-index %frames)) step))))))

(defmethod on-action-finish (action (name (eql 'sprite-animate)))
  (when (cycle-p action)
    (replace-action action 'sprite-animate)))
