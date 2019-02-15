(in-package :bloom)

(defvar *component-definitions* (au:dict #'eq))

(defclass component-data ()
  ((%type-order :accessor type-order)))

(defclass component ()
  ((%core :reader core
          :initarg :core)
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
             "Component ~s must not have an order to be both before and after ~
              the same component."
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

(defun make-component-table ()
  (au:dict #'eq
           :create-pending (au:dict #'eq)
           :created (au:dict #'eq)
           :active-by-type (au:dict #'eq)))

(defun compute-component-type-order (core)
  (flet ((dag-p (graph)
           (unless (or (cl-graph:find-edge-if graph #'cl-graph:undirected-edge-p)
                       (cl-graph:find-vertex-if
                        graph (lambda (x) (cl-graph:in-cycle-p graph x))))
             t)))
    (let ((graph (cl-graph:make-graph 'cl-graph:graph-container
                                      :default-edge-type :directed)))
      (au:do-hash (type definition *component-definitions*)
        (cl-graph:add-vertex graph type)
        (destructuring-bind (&key before after) definition
          (dolist (c before)
            (cl-graph:add-edge-between-vertexes graph type c))
          (dolist (c after)
            (cl-graph:add-edge-between-vertexes graph c type))))
      (assert (dag-p graph) ()
              "The component graph is not a DAG, therefor the type order cannot ~
               be computed.")
      (setf (type-order (component-data core))
            (mapcar #'cl-graph:element (cl-graph:topological-sort graph))))))

(defun make-component (core type &rest args)
  (let ((component (make-instance type :core core :type type)))
    (apply #'reinitialize-instance component args)
    (setf (au:href (components (active-scene core))
                   :create-pending
                   component)
          component)
    component))

(defun map-component-type (core type func)
  (let ((components (components (active-scene core))))
    (dolist (component (au:href components :active-by-type type))
      (funcall func component))))

(defun map-components (core func)
  (let ((components (components (active-scene core))))
    (dolist (component-type (type-order (component-data core)))
      (unless (eq component-type 'transform)
        (dolist (component (au:href components :active-by-type component-type))
          (funcall func component))))))

(defun get-computed-component-precedence-list (component-type)
  (au:when-let ((class (find-class component-type nil)))
    (loop :for class :in (c2mop:compute-class-precedence-list class)
          :for name = (class-name class)
          :until (eq name 'component)
          :when (subtypep name 'component)
            :collect name)))

(defun compute-component-initargs (component-type)
  (let* ((class (find-class component-type))
         (class-args (au:mappend #'c2mop:slot-definition-initargs
                                 (c2mop:class-slots
                                  (c2mop:ensure-finalized class))))
         (instance-lambda-list (c2mop:method-lambda-list
                                (first
                                 (c2mop:compute-applicable-methods-using-classes
                                  #'reinitialize-instance
                                  (list (find-class component-type))))))
         (instance-args (mapcar
                         (lambda (x)
                           (au:make-keyword
                            (car (au:ensure-list x))))
                         (rest (member '&key instance-lambda-list)))))
    (union class-args instance-args)))

(defun delete-component (component)
  (if (eq (component-type component) 'transform)
      (error "Cannot delete a transform component.")
      (progn
        (setf (state component) :destroy)
        (au:when-let ((entity (entity component)))
          (detach-component entity component)))))

;;; Component event hooks

(defgeneric on-component-create (self)
  (:method (self))
  (:method :after (self)
    (v:trace :bloom.component "Created ~a component." (component-type self))))

(defgeneric on-component-destroy (self)
  (:method (self))
  (:method :around (self)
    (call-next-method)
    (v:trace :bloom.component "Destroyed ~a component." (component-type self))))

(defgeneric on-component-attach (self)
  (:method (self))
  (:method :after (self)
    (v:trace :bloom.component "Attached ~a component to entity ~a."
             (component-type self)
             (id (entity self)))))

(defgeneric on-component-detach (self)
  (:method (self))
  (:method :around (self)
    (let ((entity (id (entity self))))
      (call-next-method)
      (setf (entity self) nil)
      (v:trace :bloom.component "Detached ~a component from entity ~a."
               (component-type self)
               entity))))

(defgeneric on-component-update (self)
  (:method (self)))

(defgeneric on-component-physics-update (self)
  (:method (self)))

(defgeneric on-component-render (self)
  (:method (self)))
