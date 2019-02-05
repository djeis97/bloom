(in-package :bloom)

(define-component actions (:after (render))
  (manager nil)
  (default-actions nil))

;;; Component event hooks

(defmethod on-component-attach ((self actions))
  (with-slots (%entity %manager %default-actions) self
    (setf %manager (make-action-manager (get-entity-component-by-type %entity 'render)
                                        %default-actions))))

(defmethod on-component-update ((self actions))
  (process-actions (manager self)))
