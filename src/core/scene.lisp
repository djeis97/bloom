(in-package :bloom)

;;; Scene definition

(defvar *scene-definitions* (au:dict #'eq))

(defun parse-scene (scene-spec)
  (let (entities prototype-map)
    (labels ((process-entity (entity)
               (destructuring-bind ((name &key prototype) . children) entity
                 (let ((prototype (or prototype name)))
                   (when (find name entities)
                     (error "Entity names cannot be duplicated: ~s" name))
                   (push name entities)
                   (push (cons name prototype) prototype-map)
                   (process-tree children))))
             (process-tree (tree)
               (map nil #'process-entity tree)))
      (process-tree scene-spec))
    (values (nreverse entities)
            (nreverse prototype-map))))

(defun parse-scene-entity-relationships (game-state scene-spec)
  (labels ((traverse (tree &optional parent)
             (destructuring-bind ((child . options) . sub-tree) tree
               (declare (ignore options))
               (let ((result (list (cons parent child))))
                 (if sub-tree
                     (append result (mapcan (lambda (x) (traverse x child)) sub-tree))
                     result)))))
    (loop :for (parent . child) :in (mapcan #'traverse scene-spec)
          :collect `(add-child ,(if parent
                                    `(get-entity-component-by-type ,parent 'transform)
                                    `(root-node (active-scene ,game-state)))
                               (get-entity-component-by-type ,child 'transform)))))

(defun collect-scene-entity-bindings (entity-names entity-table)
  (mapcan
   (lambda (x)
     `((,x (au:href ,entity-table ',x))))
   entity-names))

(defmacro define-scene (name () &body spec)
  (au:with-unique-names (game-state entity entity-name prototype entity-table)
    (au:mvlet ((entities prototypes (parse-scene spec)))
      `(setf (au:href *scene-definitions* ',name)
             (lambda (,game-state)
               (let ((,entity-table (au:dict #'eq)))
                 (dolist (,entity ',prototypes)
                   (destructuring-bind (,entity-name . ,prototype) ,entity
                     (setf (au:href ,entity-table ,entity-name)
                           (make-entity ,game-state ,entity-name :prototype ,prototype))))
                 (let ,(collect-scene-entity-bindings entities entity-table)
                   ,@(parse-scene-entity-relationships game-state spec))))))))

;;; Scenes

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

(defun make-universe (game-state)
  (let ((scene (active-scene game-state))
        (universe (%make-entity game-state nil 'universe)))
    (attach-component universe (make-component game-state 'transform :entity universe))
    (insert-entity game-state universe :parent nil)
    (setf (root-node scene) (get-entity-component-by-type universe 'transform))
    universe))

(defun make-scene (game-state scene-name)
  (let ((scene (make-instance 'scene :name scene-name)))
    (setf (active-scene game-state) scene
          (au:href (scenes game-state) scene-name) scene)
    (make-universe game-state)
    scene-name))

(defun make-scenes (game-state)
  (let ((default (option game-state :default-scene)))
    (au:do-hash-keys (name *scene-definitions*)
      (make-scene game-state name))
    (setf (active-scene game-state) nil)
    (load-scene game-state default)))

(defun switch-scene (game-state scene-name)
  (au:if-found (scene (au:href (scenes game-state) scene-name))
               (setf (next-scene game-state) scene)
               (error "Scene ~s is not defined." scene-name)))

(defun load-scene (game-state scene-name)
  (let ((scene (au:href (scenes game-state) scene-name)))
    (unless (loaded-p scene)
      (setf (active-scene game-state) scene)
      (funcall (au:href *scene-definitions* scene-name) game-state)
      (setf (loaded-p scene) t))))
