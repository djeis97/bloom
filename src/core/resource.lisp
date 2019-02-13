(in-package :bloom)

(defvar *resource-groups* (au:dict #'eq))

(defclass resource-group ()
  ((%name :reader name
          :initarg :name)
   (%project :reader project
             :initarg :project)
   (%path :reader path
          :initarg :path)))

(defmacro define-resource-paths (project &body body)
  `(setf ,@(loop :for (group path) :on (car body) :by #'cddr
                 :append `((au:href *resource-groups* ',group)
                           (make-instance 'resource-group
                                          :name ',group
                                          :project ',project
                                          :path ,path)))))

(defun resolve-path/relative (group-name file-name)
  (let ((group (au:href *resource-groups* (au:make-keyword group-name))))
    (uiop:merge-pathnames* file-name
                           (uiop:ensure-directory-pathname (path group)))))

(defun resolve-path (group-name file-name)
  (let ((group (au:href *resource-groups* (au:make-keyword group-name))))
    (uiop:merge-pathnames*
     (resolve-path/relative group-name file-name)
     (if *release-p*
         (uiop:pathname-directory-pathname
          (first (uiop:raw-command-line-arguments)))
         (asdf:system-source-directory (project group))))))

;;; Asset pools

(defvar *asset-pools* (au:dict #'eq))

(defclass asset-pool ()
  ((%name :reader name
          :initarg :name)
   (%project :reader project
             :initarg :project)
   (%type :reader asset-type
          :initarg :type)
   (%base-path :reader base-path
               :initarg :base-path)
   (%assets :reader assets
            :initform (au:dict #'eq))))

(defun make-asset-pool (name project type base-path)
  (make-instance 'asset-pool
                 :name name
                 :project project
                 :type type
                 :base-path base-path))

(defmacro define-asset-pool ((project name type &optional base-path) &body body)
  (au:with-unique-names (pool)
    `(let ((,pool (make-asset-pool ',name ',project ',type ,base-path)))
       (loop :for (asset-name file) :in ',body
             :do (pool-asset ,pool asset-name file))
       (setf (au:href *asset-pools* ',name) ,pool))))

;;; Asset

(defclass asset ()
  ((%name :reader name
          :initarg :name)
   (%pool :reader pool
          :initarg :pool)
   (%path :reader path
          :initarg :path)))

(defun make-asset (name pool path)
  (make-instance 'asset :name name :pool pool :path path))

(defun build-asset-path (pool file)
  (with-slots (%project %base-path) pool
    (let ((path (if %base-path
                    (uiop:merge-pathnames*
                     file (uiop:ensure-directory-pathname %base-path))
                    file)))
      (if (option %project :release)
          (truename
           (uiop:merge-pathnames*
            path (uiop:pathname-directory-pathname (uiop:argv0))))
          (asdf:system-relative-pathname (asdf:find-system %project) path)))))

(defun pool-asset (pool name file)
  (let ((path (build-asset-path pool file)))
    (setf (au:href (assets pool) name) (make-asset name pool path))))

;;; Asset types

(defun asset (pool name)
  (au:href (assets pool) name))
