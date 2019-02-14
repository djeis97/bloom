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
  (let ((default (option (project core) :default-scene)))
    (au:do-hash-keys (name *scene-definitions*)
      (let ((scene (make-instance 'scene :name name)))
        (setf (au:href (scenes core) name) scene)))
    (setf (active-scene core) nil)
    (load-scene core default)))

(defun switch-scene (core scene-name)
  (au:if-found (scene (au:href (scenes core) scene-name))
               (progn (load-scene core scene-name)
                      (setf (next-scene core) scene))
               (error "Scene ~s is not defined." scene-name)))

(defun load-scene (core scene-name)
  (let ((scene (au:href (scenes core) scene-name)))
    (unless (loaded-p scene)
      (setf (active-scene core) scene)
      (funcall (au:href *scene-definitions* scene-name) core)
      (setf (loaded-p scene) t))))

(defmacro define-scenes (&body body)
  `(progn
     (clrhash *scene-definitions*)
     (setf
      ,@(au:collecting
          (dolist (spec body)
            (destructuring-bind (scene-name prefab-name) spec
              (collect `(au:href *scene-definitions* ',scene-name))
              (collect `(func (find-prefab ,prefab-name)))))))))
