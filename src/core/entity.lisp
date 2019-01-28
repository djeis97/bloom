(in-package :bloom)

(defclass entity ()
  ((%id :reader id
        :initarg :id)
   (%prototype :reader prototype
               :initarg :prototype)
   (%state :accessor state
           :initform :create-pending)
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
           :create-pending (au:dict #'eq)
           :created (au:dict #'eq)
           :active-by-name (au:dict #'eq)
           :active-by-prototype (au:dict #'eq)
           :destroy-pending (au:dict #'eq)))

(defun %make-entity (game-state prototype id)
  (let ((scene (active-scene game-state))
        (entity (make-instance 'entity :id id :prototype prototype)))
    (setf (au:href (entities scene) :create-pending entity) entity)
    entity))

(defun make-entity (game-state name &key prototype)
  (assert (au:href *prototypes* prototype) ()
          "The prototype ~s does not exist when attempting to create entity ~s" prototype name)
  (let ((prototype (au:href *prototypes* prototype)))
    (funcall (func prototype) game-state name)))

(defun attach-component (entity component)
  (let ((game-state (game-state component)))
    (unless (entity component)
      (setf (entity component) entity))
    (setf (au:href (components entity) component) component
          (au:href (components-by-id entity) (component-id component)) component)
    (push component (au:href (components-by-type entity) (component-type component)))
    (mark-component-types-dirty game-state)
    (cache-component component)
    (on-component-attach component)))

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
