(in-package :bloom)

(defclass entity ()
  ((%core :reader core
          :initarg :core)
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

(defun make-entity-table ()
  (au:dict #'eq
           :create-pending (au:dict #'eq)
           :created (au:dict #'eq)
           :active-by-name (au:dict #'eq)
           :active-by-prefab (au:dict #'equalp)
           :actions (au:dict #'eq)))

(defun make-entity (core &key prefab-node)
  (let* ((id (au:unique-name
              (au:format-symbol *package* "~:@(~a~)-" (name prefab-node))))
         (entities (entities (get-current-scene core)))
         (entity (make-instance 'entity
                                :id id
                                :core core
                                :prefab-node prefab-node)))
    (setf (au:href entities :create-pending entity) entity)
    entity))

(defun attach-component (entity component)
  (with-slots (%core %id %type %entity) component
    (unless %entity
      (setf %entity entity))
    (if (au:href (components %entity) %type)
        (error "Cannot attach multiple transform components.")
        (push component (au:href (components %entity) %type)))))

(defun attach-multiple-components (entity &rest components)
  (dolist (component components)
    (attach-component entity component)))

(defun detach-component (entity component)
  (with-slots (%core %id %type) component
    (if (eq %type 'transform)
        (error "Cannot detach a transform component.")
        (progn
          (au:deletef (au:href (components entity) %type) component)
          (on-component-detach component)))))

(defun detach-all-components (entity)
  (au:do-hash-values (components (components entity))
    (dolist (component components)
      (detach-component entity component))))

(defun get-entity-component (entity component-type)
  (let ((components (au:href (components entity) component-type)))
    (values (first components)
            (rest components))))

(defun has-component-p (entity component-type)
  (when (get-entity-component entity component-type) t))

(defun insert-entity (entity &key parent)
  (let ((transform (get-entity-component entity 'transform)))
    (when parent
      (add-child (get-entity-component parent 'transform) transform))
    (on-entity-insert entity)))

(defun delete-entity (entity)
  (setf (state entity) :destroy)
  (prune-tree (get-entity-component entity 'transform))
  (on-entity-delete entity))

;;; Entity event hooks

(defgeneric on-entity-create (entity)
  (:method (entity))
  (:method :after (entity)
    (v:trace :bloom.entity "Created entity ~s." (id entity))))

(defgeneric on-entity-delete (entity)
  (:method (entity))
  (:method :after (entity)
    (v:trace :bloom.entity "Deleted entity ~s." (id entity))))

(defgeneric on-entity-insert (entity)
  (:method (entity))
  (:method :after (entity)
    (v:trace :bloom.entity "Inserted entity ~s into scene." (id entity))))
