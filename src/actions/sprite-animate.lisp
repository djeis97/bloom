(in-package :bloom)

(defclass action/sprite-animate (action) ())

(defmethod on-action-update ((action action/sprite-animate))
  (au:when-let ((sprite (get-entity-component (owner action) 'sprite))
                (step (action-step action)))
    (with-slots (%initial-index %index %frames) sprite
      (let ((i (1- (+ %initial-index %frames))))
        (setf %index (floor (au:map-domain 0 1 %initial-index i step)))))))
