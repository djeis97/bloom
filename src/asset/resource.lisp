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
    (uiop:merge-pathnames* file-name (uiop:ensure-directory-pathname (path group)))))

(defun resolve-path (group-name file-name)
  (let ((group (au:href *resource-groups* (au:make-keyword group-name))))
    (uiop:merge-pathnames*
     (resolve-path/relative group-name file-name)
     (if *release-p*
         (uiop:pathname-directory-pathname (first (uiop:raw-command-line-arguments)))
         (asdf:system-source-directory (project group))))))
