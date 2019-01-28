(in-package :bloom)

(defvar *component-definitions* (au:dict #'eq))

(defclass component-data ()
  ((%type-order :accessor type-order)
   (%cache-dirty-p :accessor cache-dirty-p
                   :initform t)))

(defclass component ()
  ((%game-state :reader game-state
                :initarg :game-state)
   (%id :accessor component-id
        :initarg :id)
   (%state :accessor state
           :initform :create-pending)
   (%type :reader component-type
          :initarg :type)
   (%entity :accessor entity
            :initarg :entity
            :initform nil)))

(defmacro define-component (name (&key before after) &body slots)
  `(progn
     (assert (not (intersection ',before ',after)) ()
             "Component ~s must not have an order to be both before and after the same component."
             ',name)
     (setf (au:href *component-definitions* ',name)
           (list :before ',before :after ',after))
     (defclass ,name (component)
       ,(loop :for (name value) :in slots
              :collect
              (append `(,(au:symbolicate '#:% name)
                        :accessor ,name
                        :initarg ,(au:make-keyword name)
                        :initform ,value))))))

(defun make-component-tables ()
  (au:dict #'eq
           :create-pending (au:dict #'eq)
           :created (au:dict #'eq)
           :active-by-type (au:dict #'eq)
           :destroy-pending (au:dict #'eq)))

(defun compute-component-type-order (game-state)
  (flet ((dag-p (graph)
           (unless (or (cl-graph:find-edge-if graph #'cl-graph:undirected-edge-p)
                       (cl-graph:find-vertex-if graph (lambda (x) (cl-graph:in-cycle-p graph x))))
             t)))
    (let ((graph (cl-graph:make-graph 'cl-graph:graph-container :default-edge-type :directed)))
      (au:do-hash (type definition *component-definitions*)
        (cl-graph:add-vertex graph type)
        (destructuring-bind (&key before after) definition
          (dolist (c before)
            (cl-graph:add-edge-between-vertexes graph type c))
          (dolist (c after)
            (cl-graph:add-edge-between-vertexes graph c type))))
      (assert (dag-p graph) ()
              "The component graph is not a DAG, therefor the type order cannot be computed.")
      (setf (type-order (component-data game-state))
            (mapcar #'cl-graph:element (cl-graph:topological-sort graph))))))

(defun cache-transform-components (game-state)
  (let* ((scene (active-scene game-state))
         (types (au:href (components scene) :active-by-type)))
    (remhash 'transform types)
    (map-nodes
     (lambda (x)
       (when x
         (au:when-let* ((entity (entity x))
                        (component (get-entity-component-by-type entity 'transform)))
           (au:appendf (au:href types 'transform) (list component)))))
     (root-node scene)))
  (setf (cache-dirty-p (component-data game-state)) nil))

(defun cache-component (component)
  (let ((types (au:href (components (active-scene (game-state component))) :active-by-type))
        (type (component-type component)))
    (unless (eq type 'transform)
      (au:appendf (au:href types type) (list component)))))

(defun uncache-component (component)
  (let ((types (au:href (components (active-scene (game-state component))) :active-by-type)))
    (au:deletef (au:href types (component-type component)) component)))

(defun mark-component-types-dirty (game-state)
  (setf (cache-dirty-p (component-data game-state)) t))

(defun make-component (game-state type &rest args)
  (let ((component (make-instance type :game-state game-state :type type)))
    (apply #'reinitialize-instance component args)
    (setf (au:href (components (active-scene game-state)) :create-pending component) component)
    component))

(defun map-components (game-state func)
  (let ((components (components (active-scene game-state))))
    (dolist (component-type (type-order (component-data game-state)))
      (dolist (component (au:href components :active-by-type component-type))
        (funcall func component)))))

;;; Component event hooks

(defgeneric on-component-create (component)
  (:method (component))
  (:method :after (component)
    (v:trace :bloom.component.create "Created ~a component." (component-type component))))

(defgeneric on-component-delete (component)
  (:method (component))
  (:method :around (component)
    (au:when-let ((entity (entity component)))
      (detach-component entity component))
    (call-next-method)
    (v:trace :bloom.component.delete "Deleted ~a component." (component-type component))))

(defgeneric on-component-attach (component)
  (:method (component))
  (:method :after (component)
    (v:trace :bloom.component.attach "Attached ~a component to entity ~a."
             (component-type component)
             (id (entity component)))))

(defgeneric on-component-detach (component)
  (:method (component))
  (:method :around (component)
    (let ((entity (id (entity component))))
      (call-next-method)
      (setf (entity component) nil)
      (v:trace :bloom.component.detach "Detached ~a component from entity ~a."
               (component-type component)
               entity))))

(defgeneric on-component-update (component)
  (:method (component)))

(defgeneric on-component-render (component)
  (:method (component)))
