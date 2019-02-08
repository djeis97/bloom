(in-package :bloom)

(defclass entity ()
  ((%game-state :reader game-state
                :initarg :game-state)
   (%id :reader id
        :initarg :id)
   (%state :accessor state
           :initform :create-pending)
   (%prefab-node :reader prefab-node
                 :initarg :prefab-node)
   (%components :reader components
                :initform (au:dict #'eq))
   (%actions :reader actions
             :initform (dll:make-dlist :test #'eq))))

(au:define-printer (entity stream :type t :identity t)
  (format stream "~a" (id entity)))

(defmacro do-entities ((game-state binding) &body body)
  `(au:do-hash-values (,binding (au:href (entities (active-scene ,game-state)) :active-by-name))
     ,@body))

(defun make-entity-table ()
  (au:dict #'eq
           :create-pending (au:dict #'eq)
           :created (au:dict #'eq)
           :active-by-name (au:dict #'eq)
           :active-by-prefab (au:dict #'equalp)
           :actions (au:dict #'eq)))

(defun make-entity (game-state &key prefab-node)
  (let* ((id (au:unique-name (au:format-symbol *package* "~:@(~a~)-" (name prefab-node))))
         (scene (active-scene game-state))
         (entity (make-instance 'entity
                                :id id
                                :game-state game-state
                                :prefab-node prefab-node)))
    (setf (au:href (entities scene) :create-pending entity) entity)
    entity))

(defun attach-component (entity component)
  (with-slots (%game-state %id %type %entity) component
    (unless %entity
      (setf %entity entity))
    (if (au:href (components %entity) %type)
        (error "Cannot attach multiple transform components.")
        (progn
          (push component (au:href (components %entity) %type))
          (mark-component-types-dirty %game-state)
          (cache-component component)))))

(defun attach-multiple-components (entity &rest components)
  (dolist (component components)
    (attach-component entity component)))

(defun detach-component (entity component)
  (with-slots (%game-state %id %type) component
    (if (eq %type 'transform)
        (error "Cannot detach a transform component.")
        (progn
          (au:deletef (au:href (components entity) %type) component)
          (uncache-component component)
          (mark-component-types-dirty %game-state)
          (on-component-detach component)))))

(defun detach-all-components (entity)
  (au:do-hash-values (components (components entity))
    (dolist (component components)
      (detach-component entity component))))

(defun get-entity-component-by-type (entity component-type)
  (let ((components (au:href (components entity) component-type)))
    (values (first components)
            (rest components))))

(defun has-component-p (entity component-type)
  (when (get-entity-component-by-type entity component-type) t))

(defun insert-entity (game-state entity &key parent)
  (let ((transform (get-entity-component-by-type entity 'transform)))
    (when parent
      (add-child (get-entity-component-by-type parent 'transform) transform))
    (mark-component-types-dirty game-state)
    (au:do-hash-values (components (components entity))
      (dolist (component components)
        (cache-component component)))
    (on-entity-insert entity)))

(defun delete-entity (entity)
  (setf (state entity) :destroy)
  (prune-tree (get-entity-component-by-type entity 'transform))
  (on-entity-delete entity))

;;; Entity event hooks

(defgeneric on-entity-create (entity)
  (:method (entity))
  (:method :after (entity)
    (v:trace :bloom.entity.create "Created entity ~s." (id entity))))

(defgeneric on-entity-delete (entity)
  (:method (entity))
  (:method :after (entity)
    (v:trace :bloom.entity.delete "Deleted entity ~s." (id entity))))

(defgeneric on-entity-insert (entity)
  (:method (entity))
  (:method :after (entity)
    (v:trace :bloom.entity.insert "Inserted entity ~s into scene." (id entity))))
