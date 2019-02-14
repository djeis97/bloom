(in-package :bloom)

(defclass action/move-tile (action)
  ((%transform :reader transform)
   (%direction :accessor direction
               :initarg :direction)))

(defmethod on-action-insert ((action action/move-tile))
  (with-slots (%owner %transform) action
    (setf %transform (get-entity-component %owner 'transform))))

(defmethod on-action-update ((action action/move-tile))
  (with-slots (%transform %direction %duration) action
    (let ((step (m:/ (m:* %direction 3) %duration)))
      (translate-transform %transform (m:vec3 step 0)))))

(defmethod on-action-finish ((action action/move-tile))
  (with-slots (%current) (translation (transform action))
    (m:with-vec3 ((v %current))
      (setf v.x (fround v.x)
            v.y (fround v.y)))))
