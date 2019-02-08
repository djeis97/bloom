(in-package :bloom)

(defclass action ()
  ((%owner :reader owner
           :initarg :owner)
   (%state :accessor state
           :initform :create-pending)
   (%location :reader location
              :initform :tail)
   (%node :accessor node)
   (%type :reader action-type
          :initarg :type)
   (%elapsed :accessor elapsed
             :initarg :elapsed
             :initform 0)
   (%duration :reader duration
              :initarg :duration
              :initform 1)
   (%finished-p :accessor finished-p
                :initarg :finished-p
                :initform nil)
   (%self-finishing-p :reader self-finishing-p
                      :initarg :self-finishing-p
                      :initform nil)
   (%blocking-p :reader blocking-p
                :initarg :blocking-p
                :initform nil)
   (%repeat-p :reader repeat-p
              :initarg :repeat-p
              :initform nil)
   (%shape :reader shape
           :initarg :shape
           :initform 'm:linear)
   (%attrs :reader attrs
           :initarg :attrs
           :initform nil)))

(defmethod initialize-instance :after ((instance action) &key &allow-other-keys)
  (with-slots (%attrs) instance
    (setf %attrs (au:plist->hash %attrs :test #'eq))))

(defun make-action-table ()
  (au:dict #'eq
           :create-pending (au:dict #'eq)
           :created (au:dict #'eq)
           :active-by-type (au:dict #'eq)))

(defun insert-action (action where &key target)
  (with-slots (%owner %type) action
    (let* ((actions (actions %owner))
           (node (dll:insert-dlist-node where actions %type action
                                        :target-key target)))
      (setf (node action) node)
      action)))

(defun remove-action (action)
  (with-slots (%owner %type) action
    (dll:remove-dlist-node (actions %owner) %type)))

(defun replace-action (action type &rest args)
  (let ((action (apply #'reinitialize-instance action
                       :type type :elapsed 0 :finished-p nil args)))
    (dll:update-dlist-node-key (node action) type)))

(defun action-step (action)
  (with-slots (%shape %elapsed %duration) action
    (funcall %shape (au:clamp (/ %elapsed %duration) 0f0 1f0))))

(defun make-action (entity &rest args)
  (let ((action-table (actions (active-scene (game-state entity))))
        (action (apply #'make-instance 'action :owner entity args)))
    (setf (au:href action-table :create-pending action) action)))

(defun process-actions (actions)
  (loop :for (type . action) :in (dll:dlist-elements actions)
        :do (on-action-update action type)
        :when (finished-p action)
          :do (on-action-finish action type)
        :when (blocking-p action)
          :do (return)))

;;; Action event hooks

(defgeneric on-action-insert (action type)
  (:method (action type)))

(defgeneric on-action-finish (action type)
  (:method (action type))
  (:method :around (action type)
    (with-slots (%owner) action
      (call-next-method)
      (v:trace :bloom.action "Action ~a finished for actor ~a."
               type (id %owner)))))

(defgeneric on-action-update (action type)
  (:method (action type))
  (:method :before (action type)
    (with-slots (%owner %elapsed %self-finishing-p %duration %finished-p) action
      (with-slots (%game-state) %owner
        (incf %elapsed (frame-time (frame-manager %game-state)))
        (when (and (not %self-finishing-p)
                   (>= %elapsed %duration))
          (setf %finished-p t))))))
