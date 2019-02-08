(in-package :bloom)

(defclass action/fade (action)
  ())

(defmethod on-action-update ((action action/fade))
  (with-slots (%owner %reverse-p) action
    (let* ((render (get-entity-component %owner 'render))
           (step (action-step action))
           (step (if %reverse-p (- 1 step) step)))
      (setf (value (au:href (uniforms (material render)) :opacity))
            step))))
