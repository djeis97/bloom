(in-package :bloom)

(defclass action/translate (action)
  ((%axis :accessor axis
          :initarg :axis
          :initform :z)
   (%offset :accessor offset
            :initarg :offset
            :initform 1.0)))

(defmethod on-action-update ((action action/translate))
  (with-slots (%owner %reverse-p %axis %offset) action
    (let* ((transform (get-entity-component %owner 'transform))
           (step (au:map-domain 0 1 0 %offset (action-step action)))
           (step (if %reverse-p (- %offset step) step)))
      (ecase %axis
        (:x (translate-transform transform (m:vec3 step 0 0) :replace-p t))
        (:y (translate-transform transform (m:vec3 0 step 0) :replace-p t))
        (:z (translate-transform transform (m:vec3 0 0 step) :replace-p t))))))
