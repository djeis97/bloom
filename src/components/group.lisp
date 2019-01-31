(in-package :bloom)

(define-component group (:after (transform)) 
  (name nil))

(defclass group-data ()
  ((%group->entities :reader group->entities
                     :initform (au:dict #'eq))
   (%entity->groups :reader entity->groups
                    :initform (au:dict #'eq))))

(defun get-group-data (game-state)
  (au:href (shared-storage game-state) :group-data))

(defun get-entity-groups (game-state entity)
  (values (au:href (entity->groups (get-group-data game-state)) (id entity))))

(defun get-group-entities (game-state group-name)
  (values (au:href (group->entities (get-group-data game-state)) group-name)))

(defun group-join (game-state entity group-name)
  (let ((id (id entity)))
    (au:when-let ((group-data (get-group-data game-state)))
      (unless (member group-name (au:href (entity->groups group-data) id))
        (push group-name (au:href (entity->groups group-data) id))
        (push id (au:href (group->entities group-data) group-name))
        (on-group-join entity group-name)))))

(defun group-leave (game-state entity group-name)
  (let ((id (id entity)))
    (au:when-let ((group-data (get-group-data game-state)))
      (au:deletef (au:href (entity->groups group-data) id) group-name)
      (au:deletef (au:href (group->entities group-data) group-name) id)
      (on-group-leave entity group-name)
      (unless (get-group-entities game-state group-name)
        (group-delete game-state group-name))
      (unless (get-entity-groups game-state id)
        (remhash entity (entity->groups group-data))))))

(defun group-leave-all (game-state entity)
  (dolist (group (get-entity-groups game-state entity))
    (group-leave game-state entity group)))

(defun group-delete (game-state group-name)
  (let ((group-data (get-group-data game-state)))
    (dolist (entity (get-group-entities game-state group-name))
      (group-leave game-state entity group-name))
    (when (member group-name (au:hash-table-keys (group->entities group-data)))
      (remhash group-name (group->entities group-data))
      (on-group-delete group-name))))

;;; Component event hooks

(defmethod on-component-create ((self group))
  (symbol-macrolet ((group-storage (au:href (shared-storage (game-state self)) :group-data)))
    (unless group-storage
      (setf group-storage (make-instance 'group-data)))))

(defmethod on-component-attach ((self group))
  (let ((name (name self))
        (entity (entity self))
        (group-data (get-group-data (game-state self))))
    (push (id entity) (au:href (group->entities group-data) name))
    (push name (au:href (entity->groups group-data) (id entity)))
    (on-group-join entity name)))

;;; Group-specific event hooks

(defgeneric on-group-join (entity group-name)
  (:method (entity group-name))
  (:method :after (entity group-name)
    (v:debug :bloom.component.group.join "Entity ~a has joined group ~s." (id entity) group-name)))

(defgeneric on-group-leave (entity group-name)
  (:method (entity group-name))
  (:method :after (entity group-name)
    (v:debug :bloom.component.group.leave "Entity ~a has left group ~s." (id entity) group-name)))

(defgeneric on-group-delete (group-name)
  (:method (group-name))
  (:method :after (group-name)
    (v:debug :bloom.component.group.delete "Deleted group ~s." group-name)))
