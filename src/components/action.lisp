(in-package :bloom)

(define-component action (:after (transform))
  (name nil)
  (renderer nil)
  (elapsed 0)
  (duration 1)
  (finished-p nil)
  (self-finishing-p nil)
  (blocking-p nil)
  (cycle-p nil)
  (shape 'linear)
  (attrs nil))

(defgeneric on-action-insert (action name)
  (:method (action name)))

(defgeneric on-action-finish (action name)
  (:method (action name))
  (:method :around (action name)
    (let ((entity (entity action)))
      (call-next-method)
      (v:trace :bloom.action.finish "Action ~s finished for entity ~s." name (id entity)))))

(defgeneric on-action-update (action name)
  (:method (action name))
  (:method :before (action name)
    (incf (elapsed action) (delta (frame-manager (game-state action))))
    (when (and (not (self-finishing-p action))
               (>= (elapsed action) (duration action)))
      (setf (finished-p action) t))))

(defun remove-action (action)
  (with-slots (%name %entity) action
    (doubly-linked-list:remove-node (actions %entity) %name)))

(defun action-step (action)
  (funcall (shape action) (au:clamp (/ (elapsed action) (duration action)) 0.0 1.0)))

(defun insert-action (action where &key replace target)
  (let ((action-list (actions (entity action)))
        (name (name action)))
    (doubly-linked-list:insert-node where action-list name action :target-key target)
    (when replace
      (doubly-linked-list:remove-nodes action-list name))
    (on-action-insert action (name action))
    action))

(defun replace-action (action name &rest args)
  (apply #'reinitialize-instance action :name name :elapsed 0 :finished-p nil args))

(defun process-actions (actions)
  (loop :for (name . action) :in (doubly-linked-list:dlist-elements actions)
        :do (on-action-update action name)
        :when (finished-p action)
          :do (on-action-finish action name)
        :when (blocking-p action)
          :do (return)))

;;; Component event hooks

(defmethod on-component-attach ((component action))
  (setf (renderer component) (get-entity-component-by-type (entity component) 'render)
        (attrs component) (au:plist->hash (attrs component) :test #'eq))
  (insert-action component :tail))
