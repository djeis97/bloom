(in-package :bloom)

(define-component render (:after (transform mesh spritesheet camera))
  (mode nil)
  (draw-method nil)
  (material nil)
  (shader nil)
  (uniforms nil)
  (transform nil))

;;; Common material uniform accessors

(defun get-render-model (render)
  (model (transform render)))

(defun get-render-view (render)
  (view (camera (get-current-scene (core render)))))

(defun get-render-projection (render)
  (projection (camera (get-current-scene (core render)))))

(defun get-render-time (render)
  (total-time (frame-manager (core render))))

(defun set-draw-method (render)
  (with-slots (%entity %draw-method %mode) render
    (let* ((type (au:ensure-symbol %mode :bloom))
           (component (get-entity-component %entity type)))
      (setf %draw-method
            (ecase %mode
              (:sprite (lambda () (funcall #'draw-sprite component)))
              (:mesh (lambda () (funcall #'draw-mesh component))))))))

;;; Component event hooks

(defmethod on-component-create ((self render))
  (with-slots (%id %material) self
    (au:if-let ((material-definition (find-material-definition %material)))
      (setf %material (make-material material-definition self))
      (error "Material ~s is not defined for component ~s." %material %id))))

(defmethod on-component-destroy ((self render))
  (with-slots (%core %material) self
    (let ((material-definition (id (definition %material)))
          (scene-materials (materials (get-current-scene %core))))
      (remhash %material (au:href scene-materials material-definition)))))

(defmethod on-component-attach ((self render))
  (with-slots (%entity %transform) self
    (setf %transform (get-entity-component %entity 'transform))
    (set-draw-method self)))

(defmethod on-component-update ((self render))
  (resolve-material (material self) self))

(defmethod on-component-render ((self render))
  (with-slots (%material) self
    (with-material (%material)
      (funcall (draw-method self)))))
