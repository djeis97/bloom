(in-package :bloom)

(define-component mesh (:before (render))
  (file nil)
  (index 0)
  (instances 1)
  (primitives nil))

(defun draw-mesh (mesh)
  (dolist (primitive (primitives mesh))
    (funcall (draw-func primitive) :instances (instances mesh))))

(defun transform-instances (game-state instances)
  (etypecase instances
    (number instances)
    ((or function symbol)
     (funcall instances game-state))))

;;; Component event hooks

(defmethod on-component-create ((self mesh))
  (with-slots (%game-state %file %index %primitives %instances) self
    (let ((data (cache-lookup %game-state :mesh-data (cons %file %index)
                  (load-mesh (resolve-path :mesh %file) :mesh-index %index))))
      (setf %instances (transform-instances %game-state %instances)
            %primitives data))))
