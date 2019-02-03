(in-package :bloom)

(defvar *scene-definitions* (au:dict #'eq))

(defclass scene ()
  ((%name :reader name
          :initarg :name)
   (%loaded-p :accessor loaded-p
              :initform nil)
   (%root-node :accessor root-node)
   (%entities :reader entities
              :initform (make-entity-tables))
   (%components :reader components
                :initform (make-component-tables))
   (%camera :accessor camera
            :initform nil)))

(defun make-scenes (game-state)
  (let ((default (option game-state :default-scene)))
    (au:do-hash-keys (name *scene-definitions*)
      (let ((scene (make-instance 'scene :name name)))
        (setf (au:href (scenes game-state) name) scene)))
    (setf (active-scene game-state) nil)
    (load-scene game-state default)))

(defun switch-scene (game-state scene-name)
  (au:if-found (scene (au:href (scenes game-state) scene-name))
               (progn (load-scene game-state scene-name)
                      (setf (next-scene game-state) scene))
               (error "Scene ~s is not defined." scene-name)))

(defun load-scene (game-state scene-name)
  (let ((scene (au:href (scenes game-state) scene-name)))
    (unless (loaded-p scene)
      (setf (active-scene game-state) scene)
      (funcall (au:href *scene-definitions* scene-name) game-state)
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
