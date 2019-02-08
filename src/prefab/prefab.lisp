(in-package :bloom)

(defvar *prefab-definitions* (au:dict #'equalp))

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%data :reader data
          :initarg :data)
   (%parse-tree :reader parse-tree
                :initform (au:dict #'equalp))
   (%root :reader root)
   (%links :reader links
           :initform (au:dict #'eq
                              :source->targets (au:dict #'equalp)
                              :target->source (au:dict #'equalp)))
   (%func :reader func
          :initform (constantly nil))))

(au:define-printer (prefab stream :type t)
  (format stream "~a" (name prefab)))

(defclass node ()
  ((%name :reader name
          :initarg :name)
   (%prefab :reader prefab
            :initarg :prefab)
   (%path :reader path
          :initarg :path)
   (%options :reader options
             :initarg :options
             :initform nil)
   (%components :reader components
                :initform nil)
   (%components-table :reader components-table
                      :initform (au:dict #'eq))
   (%parent :reader parent
            :initarg :parent
            :initform nil)
   (%children :reader children
              :initform (au:dict #'equalp))))

(au:define-printer (node stream :type t)
  (format stream "~a" (path node)))

(defun split-components/children (data)
  (flet ((children-form-p (form)
           (and (listp form)
                (typep (car form) '(and (not null) (or list string))))))
    (let ((index (or (position-if #'children-form-p data)
                     (length data))))
      (values (subseq data 0 index)
              (subseq data index)))))

(defun explode-path (path)
  (au:string-split path #\/))

(defun make-node-path (parent name)
  (au:string-merge parent "/" name))

(defun make-node-path-from-parts (path-parts)
  (format nil "/~{~a~^/~}" path-parts))

(defun %find-prefab (name)
  (au:href *prefab-definitions* name))

(defun find-prefab (name)
  (or (%find-prefab name)
      (error "Prefab ~s not found." name)))

(defun %find-node (path)
  (let* ((prefab-name (first (explode-path path)))
         (prefab (%find-prefab prefab-name)))
    (when prefab
      (au:href (parse-tree prefab) path))))

(defun find-node (path)
  (or (%find-node path)
      (error "Prefab node ~s not found." path)))

(defun map-paths (func node)
  (funcall func node)
  (au:do-hash-values (child (children node))
    (map-paths func child)))

(defun parse-copy/link (path copy-p link-p source)
  (ensure-copy/link-source-string path source)
  (ensure-copy/link-source-valid path source)
  (ensure-copy/link-source-absolute path source)
  (ensure-copy/link-source-no-trailing-slash path source)
  (values path
          (list :mode (cond (copy-p 'copy) (link-p 'link))
                :source source)))

(defun parse-path-spec (parent path-spec)
  (labels ((check-path (path)
             (ensure-path-string path)
             (ensure-path-length parent path)
             (ensure-path-valid path)
             (ensure-path-relative path)
             (ensure-path-no-trailing-slash path))
           (direct-path (path)
             (check-path path)
             (values (make-node-path parent path)
                     (list :mode 'direct))))
    (typecase path-spec
      (string
       (direct-path path-spec))
      (list
       (destructuring-bind (target options) path-spec
         (check-path target)
         (let ((path (make-node-path parent target)))
           (ensure-path-options-plist path options)
           (ensure-path-options-valid path options)
           (destructuring-bind (&key (copy nil copy-p) (link nil link-p)) options
             (ensure-path-options-keys path options)
             (au:if-let ((copy/link-form (or copy link)))
               (parse-copy/link path copy-p link-p copy/link-form)
               (direct-path target)))))))))

(defun make-node (prefab path)
  (symbol-macrolet ((node (au:href (parse-tree prefab) path)))
    (unless node
      (let ((name (first (last (explode-path path)))))
        (setf node (make-instance 'node
                                  :name name
                                  :prefab prefab
                                  :path path
                                  :options '(:mode direct)))))
    node))

(defun make-paths (prefab path data &optional options)
  (au:mvlet ((components children (split-components/children data))
             (node (make-node prefab path)))
    (declare (ignore components))
    (when options
      (setf (slot-value node '%options) options))
    (dolist (child children)
      (destructuring-bind (path-spec . body) child
        (au:mvlet ((path options (parse-path-spec path path-spec)))
          (make-paths prefab path body options))))))

(defun evaluate-components (components)
  (let ((specs))
    (dolist (component components)
      (destructuring-bind (type options . args) component
        (loop :for (k v) :on args :by #'cddr
              :append (list k (eval v)) :into new-args
              :finally (push `(,type ,options ,@new-args) specs))))
    specs))

(defun add-components (prefab path data)
  (au:mvlet ((components children (split-components/children data)))
    (let ((node (make-node prefab path)))
      (with-slots (%components) node
        (au:appendf %components (evaluate-components components))
        (dolist (child children)
          (destructuring-bind (path-spec . body) child
            (let ((path (parse-path-spec path path-spec)))
              (add-components prefab path body))))))))

(defun make-parse-tree (prefab data)
  (with-slots (%name %root) prefab
    (let ((path (make-node-path nil %name)))
      (make-paths prefab path data)
      (add-components prefab path data)
      (setf %root (find-node path)))))

(defun expand-parse-tree-path (prefab path-parts)
  (au:when-let* ((path-parts (butlast path-parts))
                (path (format nil "/~{~a~^/~}" path-parts)))
    (make-node prefab path)
    (expand-parse-tree-path prefab path-parts)))

(defun expand-parse-tree (prefab)
  (au:do-hash-keys (path (parse-tree prefab))
    (expand-parse-tree-path prefab (explode-path path))))

(defun verify-components (prefab)
  (au:do-hash (path node (parse-tree prefab))
    (dolist (component (components node))
      (ensure-component-list component path)
      (ensure-component-form component path)
      (destructuring-bind (type options . args) component
        (ensure-component-type-symbol type path)
        (ensure-component-type-exists type path)
        (ensure-component-options-plist type options path)
        (ensure-component-options-valid type options path)
        (ensure-component-id type options path)
        (ensure-component-policy type options path)
        (ensure-component-args-plist type args path)
        (ensure-component-args-valid type args path)))))

(defun insert-missing-transforms (prefab)
  (au:do-hash (path node (parse-tree prefab))
    (with-slots (%components) node
      (unless (find 'transform %components :key #'car)
        (push '(transform (:policy :old-type)) %components))
      (ensure-path-single-transform %components path))))

(defun collect-source-components (node)
  (let ((components))
    (au:do-hash (type table (components-table node))
      (au:do-hash-values (data table)
        (destructuring-bind (&key id args &allow-other-keys) data
          (push `(,type (:id ,id) ,@args) components))))
    components))

(defun insert-source-components (source target)
  (with-slots (%components) target
    (setf %components (append (collect-source-components source) %components))))

(defun copy-source-nodes (prefab)
  (au:do-hash (path node (parse-tree prefab))
    (destructuring-bind (&key source &allow-other-keys) (options node)
      (when source
        (map-paths
         (lambda (x)
           (if (string= source (path x))
               (insert-source-components x node)
               (let* ((sub-path (subseq (path x) (1+ (length source))))
                      (new-path (make-node-path path sub-path))
                      (new-node (make-node prefab new-path)))
                 (insert-source-components x new-node))))
         (find-node source))))))

(defun make-relationships (prefab)
  (flet ((get-parent (path)
           (au:when-let ((path-parts (butlast (explode-path path))))
             (make-node-path-from-parts path-parts))))
    (au:do-hash (path node (parse-tree prefab))
      (au:when-let ((parent-path (get-parent path)))
        (setf (slot-value node '%parent) (find-node parent-path)
              (au:href (children (parent node)) path) (find-node path))))))

(defun get-source-prefab (node)
  (destructuring-bind (&key source &allow-other-keys) (options node)
    (let ((name (first (explode-path source))))
      (find-prefab name))))

(defun make-links (prefab)
  (au:do-hash (path node (parse-tree prefab))
    (destructuring-bind (&key mode source &allow-other-keys) (options node)
      (when (eq mode 'link)
        (let* ((source-prefab (get-source-prefab node))
               (links (au:href (links source-prefab))))
          (symbol-macrolet ((targets (au:href links :source->targets source)))
            (setf (au:href links :target->source path) source)
            (unless targets
              (setf targets (au:dict #'equalp)))
            (setf (au:href targets path) node)))))))

(defun remove-broken-links (prefab)
  (let ((source->targets (au:href (links prefab) :source->targets))
        (target->source (au:href (links prefab) :target->source)))
    (au:do-hash (target source target->source)
      (unless (%find-node target)
        (remhash target target->source)
        (remhash target (au:href source->targets source))
        (unless (au:href source->targets source)
          (remhash source source->targets))))
    (au:do-hash-keys (source source->targets)
      (unless (%find-node source)
        (remhash source source->targets)
        (au:do-hash (k v target->source)
          (when (string= v source)
            (remhash k target->source)))))))

(defun reinitialize-prefab (prefab)
  (clrhash (parse-tree prefab))
  (parse-prefab prefab)
  prefab)

(defun update-links-recursively (prefab)
  (au:do-hash-keys (target (au:href (links prefab) :target->source))
    (let* ((name (first (explode-path target)))
           (target-prefab (find-prefab name)))
      (reinitialize-prefab target-prefab)
      (update-links-recursively target-prefab))))

(defun update-links (prefab)
  (make-links prefab)
  (remove-broken-links prefab)
  (update-links-recursively prefab))

(defgeneric merge-component (policy node type id args)
  (:method ((policy null) node type id args)
    (ensure-component-not-duplicate node type id)
    (merge-component :new-type node type id args)))

(defmethod merge-component ((policy (eql :new-type)) node type id args)
  (setf (au:href (components-table node) type id)
        (list :id id :policy policy :args args)))

(defmethod merge-component ((policy (eql :old-type)) node type id args)
  (au:unless-found (components (au:href (components-table node) type id))
    (setf (au:href (components-table node) type id)
          (list :id id :policy policy :args args))))

(defmethod merge-component ((policy (eql :new-args)) node type id args)
  (let* ((old-args (au:plist->hash
                    (getf (au:href (components-table node) type id) :args)))
         (new-args (au:hash->plist
                    (au:merge-tables old-args (au:plist->hash args)))))
    (setf (au:href (components-table node) type id)
          (list :id id :policy policy :args new-args))))

(defmethod merge-component ((policy (eql :old-args)) node type id args)
  (let* ((old-args (au:plist->hash
                    (getf (au:href (components-table node) type id) :args)))
         (new-args (au:hash->plist
                    (au:merge-tables (au:plist->hash args) old-args))))
    (setf (au:href (components-table node) type id)
          (list :id id :policy policy :args new-args))))

(defun make-prefab-component-table (prefab)
  (au:do-hash-values (node (parse-tree prefab))
    (dolist (component (components node))
      (destructuring-bind (type (&key (id 0) policy) . args) component
        (unless (au:href (components-table node) type)
          (setf (au:href (components-table node) type) (au:dict #'eql)))
        (merge-component policy node type id args)))))

(defun print-prefab (name)
  (flet ((print-line (level value)
           (format t "~a~v@<~s~>~%"
                   (make-string level :initial-element #\Space)
                   level value)))
    (map-paths
     (lambda (x)
       (let ((level (* 3 (1- (length (explode-path (path x)))))))
         (print-line level (name x))
         (au:do-hash (type table (components-table x))
           (au:do-hash-values (data table)
             (print-line level `(,type ,@(getf data :args)))))
         (format t "~%")))
     (root (find-prefab name)))))

(defun parse-prefab (prefab)
  (let ((success-p))
    (with-slots (%name %data) prefab
      (unwind-protect
           (progn
             (make-parse-tree prefab %data)
             (expand-parse-tree prefab)
             (verify-components prefab)
             (insert-missing-transforms prefab)
             (copy-source-nodes prefab)
             (make-prefab-component-table prefab)
             (make-relationships prefab)
             (update-links prefab)
             (setf success-p t))
        (unless success-p
          (remhash %name *prefab-definitions*))))))

(defun make-prefab (name data)
  (let ((prefab (or (%find-prefab name)
                    (make-instance 'prefab :name name))))
    (with-slots (%data %parse-tree) prefab
      (setf %data data)
      (clrhash %parse-tree))
    prefab))

(defun make-prefab-entities (game-state prefab)
  (let ((prefab-entities (au:dict #'equalp))
        (scene-entities (entities (active-scene game-state))))
    (symbol-macrolet ((active (au:href scene-entities :active-by-prefab
                                       (name prefab))))
      (unless active
        (setf active (au:dict #'eq)))
      (au:do-hash (path node (parse-tree prefab))
        (let ((entity (make-entity game-state :prefab-node node)))
          (setf (au:href prefab-entities path) entity
                (au:href active (id entity)) entity)))
      prefab-entities)))

(defun make-prefab-entity-components (game-state entities)
  (au:do-hash-values (entity entities)
    (dolist (type (type-order (component-data game-state)))
      (au:when-let (table (au:href (components-table (prefab-node entity)) type))
        (au:do-hash-values (data table)
          (let ((component (apply #'make-component game-state type :id type
                                  (getf data :args))))
            (attach-component entity component)))))))

(defun make-prefab-entity-relationships (game-state prefab entities)
  (labels ((get-transform (node)
             (get-entity-component (au:href entities (path node)) 'transform))
           (get-root-node ()
             (let ((root-node (get-transform (root prefab))))
               (setf (root-node (active-scene game-state)) root-node)
               root-node)))
    (au:do-hash-values (entity entities)
      (let ((node (prefab-node entity)))
        (au:do-hash-values (child (children node))
          (add-child
           (if (parent node)
               (get-transform (parent child))
               (get-root-node))
           (get-transform child)))))
    entities))

(defun remove-prefab-entities (game-state prefab)
  (do-entities (game-state entity)
    (with-slots (%prefab-node) entity
      (au:do-hash-keys (path (parse-tree prefab))
        (when (and %prefab-node (string= (path %prefab-node) path))
          (delete-entity entity))))))

(defun make-prefab-factory (prefab)
  (lambda (game-state)
    (let ((entities (make-prefab-entities game-state prefab)))
      (make-prefab-entity-components game-state entities)
      (make-prefab-entity-relationships game-state prefab entities))))

(defmacro define-prefab (name &body body)
  (au:with-unique-names (prefab)
    `(progn
       (ensure-prefab-name-string ',name)
       (ensure-prefab-name-valid ',name)
       (let ((,prefab (make-prefab ',name ',body)))
         (setf (au:href *prefab-definitions* ',name) ,prefab)
         (parse-prefab ,prefab)
         (setf (slot-value ,prefab '%func) (make-prefab-factory ,prefab))))))
