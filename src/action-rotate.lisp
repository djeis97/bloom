(in-package :bloom)

(defclass action/rotate (action)
  ((%axis :accessor axis
          :initarg :axis
          :initform :z)
   (%angle :accessor angle
           :initarg :angle
           :initform (* pi 2))))

(defmethod on-action-update ((action action/rotate))
  (with-slots (%owner %axis %angle %reverse-p) action
    (let* ((transform (get-entity-component %owner 'transform))
           (step (au:map-domain 0 1 0 %angle (action-step action)))
           (step (if %reverse-p (- step) step)))
      (ecase %axis
        (:x (rotate-transform transform (m:vec3 step 0 0)))
        (:y (rotate-transform transform (m:vec3 0 step 0)))
        (:z (rotate-transform transform (m:vec3 0 0 step)))))))
