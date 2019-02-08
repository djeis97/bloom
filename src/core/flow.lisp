(in-package :bloom)

(defun flow/update (game-state)
  (flow/switch-scene game-state)
  (flow/create game-state)
  (flow/delete game-state))

;; switch scene

(defun flow/switch-scene (game-state)
  (with-slots (%active-scene %next-scene) game-state
    (when %next-scene
      (unless (eq %active-scene %next-scene)
        (setf %active-scene %next-scene)))))

;; create

(defun flow/create (game-state)
  (let* ((scene (active-scene game-state))
         (entity-table (entities scene))
         (component-table (components scene))
         (action-table (actions scene)))
    (when (plusp (hash-table-count (au:href action-table :create-pending)))
      (flow/create/actions game-state))
    (when (or (plusp (hash-table-count (au:href entity-table :create-pending)))
              (plusp (hash-table-count (au:href component-table :create-pending))))
      (flow/create/components game-state))))

(defun flow/create/actions (game-state)
  (let ((action-table (actions (active-scene game-state))))
    (au:do-hash-values (v (au:href action-table :create-pending))
      (let ((entity (owner v)))
        (remhash v (au:href action-table :create-pending))
        (setf (au:href action-table :created v) v
              (state v) :created)
        (unless (get-entity-component-by-type entity 'actions)
          (attach-component entity (make-component game-state 'actions)))))))

(defun flow/create/components (game-state)
  (let ((component-table (components (active-scene game-state))))
    (dolist (type (type-order (component-data game-state)))
      (au:do-hash-values (v (au:href component-table :create-pending))
        (when (eq (component-type v) type)
          (remhash v (au:href component-table :create-pending))
          (setf (au:href component-table :created v) v
                (state v) :created)
          (on-component-create v))))
    (flow/create/entities game-state)))

(defun flow/create/entities (game-state)
  (let ((entity-table (entities (active-scene game-state))))
    (au:do-hash-values (v (au:href entity-table :create-pending))
      (remhash v (au:href entity-table :create-pending))
      (setf (au:href entity-table :created v) v
            (state v) :created))
    (flow/create/activate-components game-state)))

(defun flow/create/activate-components (game-state)
  (let ((component-table (components (active-scene game-state))))
    (au:do-hash-values (v (au:href component-table :created))
      (remhash v (au:href component-table :created))
      (pushnew v (au:href component-table :active-by-type (component-type v)))
      (setf (state v) :active)
      (on-component-attach v))
    (flow/create/activate-entities game-state)))

(defun flow/create/activate-entities (game-state)
  (let ((entity-table (entities (active-scene game-state))))
    (au:do-hash-values (v (au:href entity-table :created))
      (remhash v (au:href entity-table :created))
      (setf (au:href entity-table :active-by-name (id v)) v
            (state v) :active))
    (flow/create/activate-actions game-state)))

(defun flow/create/activate-actions (game-state)
  (let ((action-table (actions (active-scene game-state))))
    (au:do-hash-values (v (au:href action-table :created))
      (with-slots (%type %owner %state %manager %location) v
        (remhash v (au:href action-table :created))
        (pushnew v (au:href action-table :active-by-type %type))
        (setf %state :active
              %manager (manager (get-entity-component-by-type %owner 'actions)))
        (apply #'insert-action v (au:ensure-list %location))
        (on-action-insert v %type)))))

;;; delete

(defun flow/delete (game-state)
  (flow/delete/entities game-state)
  (flow/delete/components game-state))

(defun flow/delete/entities (game-state)
  (let ((entity-table (entities (active-scene game-state))))
    (au:do-hash (k entity (au:href entity-table :active-by-name))
      (when (eq (state entity) :destroy)
        (au:do-hash-values (components (components entity))
          (dolist (component components)
            (setf (state component) :destroy)))
        (remhash k (au:href entity-table :active-by-name))
        (let ((prefab-name (name (prefab (prefab-node entity)))))
          (remhash k (au:href entity-table :active-by-prefab prefab-name)))))))

(defun flow/delete/components (game-state)
  (let ((component-table (components (active-scene game-state))))
    (au:do-hash (k v (au:href component-table :active-by-type))
      (dolist (component v)
        (when (eq (state component) :destroy)
          (au:deletef (au:href component-table :active-by-type k) component)
          (on-component-destroy component)))
      (unless v
        (remhash k (au:href component-table :active-by-type))))))
