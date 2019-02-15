(in-package :bloom)

(defvar *scene-definitions* (au:dict #'eq))

(defclass scene ()
  ((%name :reader name
          :initarg :name)
   (%loaded-p :accessor loaded-p
              :initform nil)
   (%root-node :accessor root-node)
   (%materials :reader materials
               :initform (au:dict #'eq))
   (%entities :reader entities
              :initform (make-entity-table))
   (%components :reader components
                :initform (make-component-table))
   (%actions :reader actions
             :initform (make-action-table))
   (%camera :accessor camera
            :initform nil)))

(defun make-scenes (core)
  (with-slots (%project %scenes) core
    (let ((default (option %project :default-scene)))
      (au:do-hash-keys (name *scene-definitions*)
        (let ((scene (make-instance 'scene :name name)))
          (setf (au:href (table %scenes) name) scene)))
      (setf (current %scenes) nil)
      (load-scene core default))))

(defun switch-scene (core scene-name)
  (with-slots (%table %next) (scenes core)
    (au:if-found (scene (au:href %table scene-name))
                 (progn (load-scene core scene-name)
                        (setf %next scene))
                 (error "Scene ~s is not defined." scene-name))))

(defun load-scene (core scene-name)
  (with-slots (%table %current) (scenes core)
    (let ((scene (au:href %table scene-name)))
      (unless (loaded-p scene)
        (setf %current scene)
        (funcall (au:href *scene-definitions* scene-name) core)
        (setf (loaded-p scene) t)))))

(defmacro define-scenes (&body body)
  `(progn
     (clrhash *scene-definitions*)
     (setf
      ,@(au:collecting
          (dolist (spec body)
            (destructuring-bind (scene-name prefab-name) spec
              (collect `(au:href *scene-definitions* ',scene-name))
              (collect `(func (find-prefab ,prefab-name)))))))))

(defclass scene-manager ()
  ((%current :accessor current
             :initform nil)
   (%next :accessor next
          :initform nil)
   (%table :reader table
           :initform (au:dict #'eq))))

(defun get-current-scene (core)
  (current (scenes core)))
