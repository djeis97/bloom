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
  (view (camera (active-scene (game-state render)))))

(defun get-render-projection (render)
  (projection (camera (active-scene (game-state render)))))

(defun get-render-time (render)
  (total-time (frame-manager (game-state render))))

(defun set-draw-method (render)
  (with-slots (%entity %draw-method %mode) render
    (let* ((type (au:format-symbol :bloom "~a" %mode))
           (component (get-entity-component-by-type %entity type)))
      (setf %draw-method
            (ecase %mode
              (:sprite (lambda () (funcall #'draw-sprite component)))
              (:mesh (lambda () (funcall #'draw-mesh component))))))))

;;; Component event hooks

(defmethod on-component-create ((component render))
  (with-slots (%id %material) component
    (au:if-let ((material-definition (find-material-definition %material)))
      (setf %material (make-material material-definition component))
      (error "Material ~s is not defined for component ~s." %material %id))))

(defmethod on-component-attach ((component render))
  (with-slots (%entity %transform) component
    (setf %transform (get-entity-component-by-type %entity 'transform))
    (set-draw-method component)))

(defmethod on-component-update ((component render))
  (resolve-material (material component) component))

(defmethod on-component-render ((component render))
  (with-slots (%updated-p %material) component
    (with-material (%material)
      (funcall (draw-method component)))))
