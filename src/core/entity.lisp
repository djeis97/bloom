(in-package :bloom)

(defclass entity ()
  ((%id :reader id
        :initarg :id)
   (%state :accessor state
           :initform :create-pending)
   (%prefab-node :reader prefab-node
                 :initarg :prefab-node)
   (%actions :reader actions
             :initform (doubly-linked-list:make-dlist :test #'eq))
   (%components :reader components
                :initform (au:dict #'eq))
   (%components-by-type :reader components-by-type
                        :initform (au:dict #'eq))
   (%components-by-id :reader components-by-id
                      :initform (au:dict #'eq))))

(au:define-printer (entity stream :type t :identity t)
  (format stream "~a" (id entity)))

(defun make-entity-tables ()
  (au:dict #'eq
           :create-pending (au:dict #'equalp)
           :created (au:dict #'equalp)
           :active-by-name (au:dict #'equalp)
           :destroy-pending (au:dict #'equalp)))

(defun make-entity (game-state &key id prefab-node)
  (let* ((id (or id (and prefab-node (name prefab-node))))
         (scene (active-scene game-state))
         (entity (make-instance 'entity :id id :prefab-node prefab-node)))
    (setf (au:href (entities scene) :create-pending entity) entity)
    entity))

(defun attach-component (entity component)
  (let ((game-state (game-state component)))
    (unless (entity component)
      (setf (entity component) entity))
    (setf (au:href (components entity) component) component
          (au:href (components-by-id entity) (component-id component)) component)
    (push component (au:href (components-by-type entity) (component-type component)))
    (mark-component-types-dirty game-state)
    (cache-component component)
    (map-components game-state #'on-component-attach)))

(defun attach-multiple-components (entity &rest components)
  (dolist (component components)
    (attach-component entity component)))

(defun detach-component (entity component)
  (let ((game-state (game-state component)))
    (remhash component (components entity))
    (remhash (component-id component) (components-by-id entity))
    (au:deletef (au:href (components-by-type entity) (component-type component)) component)
    (uncache-component component)
    (mark-component-types-dirty game-state)
    (on-component-detach component)))

(defun detach-all-components (entity)
  (au:do-hash-keys (component (components entity))
    (detach-component entity component)))

(defun get-entity-component-by-type (entity component-type)
  (let ((components (au:href (components-by-type entity) component-type)))
    (values (first components)
            (rest components))))

(defun get-entity-component-by-id (entity component-id)
  (au:href (components-by-id entity) component-id))

(defun has-component-p (entity component-type)
  (when (get-entity-component-by-type entity component-type) t))

(defun toggle-component (entity component)
  (au:if-let ((attached (au:href (components-by-type entity) (component-type component))))
    (dolist (c attached)
      (detach-component entity c))
    (attach-component entity component)))

(defun insert-entity (game-state entity &key parent)
  (let ((transform (get-entity-component-by-type entity 'transform)))
    (when parent
      (if (eq parent :universe)
          (add-child (root-node (active-scene game-state)) transform)
          (add-child (get-entity-component-by-type parent 'transform) transform)))
    (mark-component-types-dirty game-state)
    (au:do-hash-keys (component (components entity))
      (cache-component component))
    (on-entity-insert entity)))

(defun delete-entity (game-state entity)
  ;; TODO: Actually delete entity.
  (declare (ignore game-state))
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
