(in-package :bloom)

(defun flow/update (game-state)
  (flow/update/switch-scene game-state))

(defun flow/update/switch-scene (game-state)
  (with-slots (%active-scene %next-scene) game-state
    (when %next-scene
      (unless (eq %active-scene %next-scene)
        (setf %active-scene %next-scene)))
    (flow/update/check-pending game-state)))

(defun flow/update/check-pending (game-state)
  (let* ((scene (active-scene game-state))
         (entity-table (entities scene))
         (component-table (components scene)))
    (when (or (plusp (hash-table-count (au:href entity-table :create-pending)))
              (block nil
                (au:do-hash-values (v (au:href component-table :create-pending))
                  (when (plusp (hash-table-count v))
                    (return-from nil t)))))
      (flow/update/create-components game-state))))

(defun flow/update/create-components (game-state)
  (let ((component-table (components (active-scene game-state))))
    (dolist (type (type-order (component-data game-state)))
      (au:do-hash-values (v (au:href component-table :create-pending))
        (when (eq (component-type v) type)
          (remhash v (au:href component-table :create-pending))
          (setf (au:href component-table :created v) v
                (state v) :created)
          (on-component-create v))))
    (flow/update/create-entities game-state)))

(defun flow/update/create-entities (game-state)
  (let ((entity-table (entities (active-scene game-state))))
    (au:do-hash-values (v (au:href entity-table :create-pending))
      (remhash v (au:href entity-table :create-pending))
      (setf (au:href entity-table :created v) v
            (state v) :created))
    (flow/update/activate-components game-state)))

(defun flow/update/activate-components (game-state)
  (let ((component-table (components (active-scene game-state))))
    (au:do-hash-values (v (au:href component-table :created))
      (remhash v (au:href component-table :created))
      (pushnew v (au:href component-table :active-by-type (component-type v)))
      (setf (state v) :active))
    (flow/update/activate-entities game-state)))

(defun flow/update/activate-entities (game-state)
  (let ((entity-table (entities (active-scene game-state))))
    (au:do-hash-values (v (au:href entity-table :created))
      (remhash v (au:href entity-table :created))
      (setf (au:href entity-table :active-by-name (id v)) v
            (au:href entity-table :active-by-prototype (prototype v)) v
            (state v) :active))
    (flow/update/check-pending game-state)))
