(in-package :bloom)

(define-component tag (:after (transform))
  (name nil))

(defclass tag-data ()
  ((%tag->entity :reader tag->entity
                 :initform (au:dict #'eq))
   (%entity->tag :reader entity->tag
                 :initform (au:dict #'eq))))

(defun get-tag-data (game-state)
  (au:href (shared-storage game-state) :tag-data))

(defun get-entity-tag (game-state entity)
  (values (au:href (entity->tag (get-tag-data game-state)) (id entity))))

(defun get-tag-entity (game-state tag-name)
  (values (au:href (tag->entity (get-tag-data game-state)) tag-name)))

(defun tag-entity (game-state entity tag-name)
  (attach-component entity (make-component game-state 'tag :name tag-name))
  (on-entity-tag entity tag-name))

(defun untag-entity (game-state entity)
  (let* ((id (id entity))
         (tag-data (get-tag-data game-state))
         (tag-name (au:href (entity->tag tag-data) id)))
    (when tag-name
      (remhash id (au:href (entity->tag tag-data)))
      (remhash tag-name (au:href (tag->entity tag-data)))
      (on-entity-untag entity tag-name))))

;;; Component event hooks

(defmethod on-component-create ((self tag))
  (symbol-macrolet ((tag-storage (au:href (shared-storage (game-state self)) :tag-data)))
    (unless tag-storage
      (setf tag-storage (make-instance 'tag-data)))))

(defmethod on-component-attach ((self tag))
  (let* ((game-state (game-state self))
         (name (name self))
         (entity (entity self))
         (tag-data (get-tag-data game-state)))
    (let ((existing (nth-value 1 (get-entity-component-by-type entity 'tag))))
      (dolist (x existing)
        (detach-component entity x)))
    (setf (au:href (tag->entity tag-data) name) entity
          (au:href (entity->tag tag-data) (id entity)) name)
    (on-entity-tag entity name)))

(defmethod on-component-detach ((self tag))
  (untag-entity (game-state self) (entity self)))

;;; Tag-specific event hooks

(defgeneric on-entity-tag (entity tag-name)
  (:method (entity tag-name))
  (:method :after (entity tag-name)
    (v:trace :bloom.component.tag.added "Added tag ~s to entity ~a." tag-name (id entity))))

(defgeneric on-entity-untag (entity tag-name)
  (:method (entity tag-name))
  (:method :after (entity tag-name)
    (v:trace :bloom.component.tag.remove "Removed tag ~s from entity ~a." tag-name (id entity))))
