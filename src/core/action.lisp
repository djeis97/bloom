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
   (%blocking-p :reader blocking-p
                :initarg :blocking-p
                :initform nil)
   (%repeat-p :reader repeat-p
              :initarg :repeat-p
              :initform nil)
   (%reverse-p :accessor reverse-p
               :initarg :reverse-p
               :initform nil)
   (%shape :reader shape
           :initarg :shape
           :initform 'm:linear)))

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
      (on-action-insert action)
      action)))

(defun remove-action (action)
  (with-slots (%owner %type) action
    (dll:remove-dlist-node (actions %owner) %type)))

(defun action-step (action)
  (with-slots (%shape %elapsed %duration) action
    (funcall %shape (* 2 (au:clamp (/ %elapsed %duration) 0f0 1f0)))))

(defun make-action (entity &rest args)
  (let ((action-table (actions (active-scene (game-state entity))))
        (action (apply #'make-instance 'action
                       :owner entity
                       :allow-other-keys t
                       args)))
    (apply #'change-class action (getf args :type) args)
    (setf (au:href action-table :create-pending action) action)))

(defun process-actions (actions)
  (loop :for (nil . action) :in (dll:dlist-elements actions)
        :do (on-action-update action)
        :when (finished-p action)
          :do (on-action-finish action)
        :when (blocking-p action)
          :do (return)))

;;; Action event hooks

(defgeneric on-action-insert (action)
  (:method (action)))

(defgeneric on-action-finish (action)
  (:method (action))
  (:method :around (action)
    (with-slots (%type %owner %elapsed %finished-p %repeat-p %reverse-p) action
      (when %repeat-p
        (setf %reverse-p (not %reverse-p)
              %elapsed 0
              %finished-p nil))
      (call-next-method)
      (v:trace :bloom.action "Action ~a finished for actor ~a."
               %type (id %owner))
      (unless %repeat-p
        (remove-action action)))))

(defgeneric on-action-update (action)
  (:method (action))
  (:method :before (action)
    (with-slots (%owner %elapsed %duration %finished-p) action
      (with-slots (%game-state) %owner
        (incf %elapsed (frame-time (frame-manager %game-state)))
        (when (>= %elapsed %duration)
          (setf %finished-p t))))))
