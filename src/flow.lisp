(in-package :bloom)

(defun flow/update (core)
  (flow/switch-scene core)
  (flow/actions core)
  (flow/create core)
  (flow/delete core))

;; switch scene

(defun flow/switch-scene (core)
  (with-slots (%active-scene %next-scene) core
    (when %next-scene
      (unless (eq %active-scene %next-scene)
        (setf %active-scene %next-scene)))))

;; actions

(defun flow/actions (core)
  (let ((action-table (actions (active-scene core))))
    (au:do-hash-values (v (au:href action-table :create-pending))
      (remhash v (au:href action-table :create-pending))
      (setf (au:href action-table :created v) v
            (state v) :created))
    (flow/actions/activate core)))

(defun flow/actions/activate (core)
  (let ((entity-table (entities (active-scene core)))
        (action-table (actions (active-scene core))))
    (au:do-hash-values (v (au:href action-table :created))
      (with-slots (%type %owner %state %location) v
        (remhash v (au:href action-table :created))
        (pushnew v (au:href action-table :active-by-type %type))
        (setf (au:href entity-table :actions %owner) (actions %owner)
              %state :active)
        (apply #'insert-action v (au:ensure-list %location))))
    (flow/actions/process core)))

(defun flow/actions/process (core)
  (let ((entity-table (entities (active-scene core))))
    (au:do-hash-values (v (au:href entity-table :actions))
      (process-actions v))))

;; create

(defun flow/create (core)
  (let* ((scene (active-scene core))
         (entity-table (entities scene))
         (component-table (components scene)))
    (when (or (plusp (hash-table-count (au:href entity-table :create-pending)))
              (plusp (hash-table-count
                      (au:href component-table :create-pending))))
      (flow/create/components core))))

(defun flow/create/components (core)
  (let ((component-table (components (active-scene core))))
    (dolist (type (type-order (component-data core)))
      (au:do-hash-values (v (au:href component-table :create-pending))
        (when (eq (component-type v) type)
          (remhash v (au:href component-table :create-pending))
          (setf (au:href component-table :created v) v
                (state v) :created)
          (on-component-create v))))
    (flow/create/entities core)))

(defun flow/create/entities (core)
  (let ((entity-table (entities (active-scene core))))
    (au:do-hash-values (v (au:href entity-table :create-pending))
      (remhash v (au:href entity-table :create-pending))
      (setf (au:href entity-table :created v) v
            (state v) :created))
    (flow/create/activate-components core)))

(defun flow/create/activate-components (core)
  (let ((component-table (components (active-scene core))))
    (au:do-hash-values (v (au:href component-table :created))
      (remhash v (au:href component-table :created))
      (pushnew v (au:href component-table :active-by-type (component-type v)))
      (setf (state v) :active)
      (on-component-attach v))
    (flow/create/activate-entities core)))

(defun flow/create/activate-entities (core)
  (let ((entity-table (entities (active-scene core))))
    (au:do-hash-values (v (au:href entity-table :created))
      (remhash v (au:href entity-table :created))
      (setf (au:href entity-table :active-by-name (id v)) v
            (state v) :active))))

;;; delete

(defun flow/delete (core)
  (flow/delete/entities core)
  (flow/delete/components core))

(defun flow/delete/entities (core)
  (let ((entity-table (entities (active-scene core))))
    (au:do-hash (k entity (au:href entity-table :active-by-name))
      (when (eq (state entity) :destroy)
        (au:do-hash-values (components (components entity))
          (dolist (component components)
            (setf (state component) :destroy)))
        (remhash k (au:href entity-table :active-by-name))
        (let ((prefab-name (name (prefab (prefab-node entity)))))
          (remhash k (au:href entity-table :active-by-prefab prefab-name)))))))

(defun flow/delete/components (core)
  (let ((component-table (components (active-scene core))))
    (au:do-hash (k v (au:href component-table :active-by-type))
      (dolist (component v)
        (when (eq (state component) :destroy)
          (au:deletef (au:href component-table :active-by-type k) component)
          (on-component-destroy component)))
      (unless v
        (remhash k (au:href component-table :active-by-type))))))
