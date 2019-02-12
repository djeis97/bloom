(in-package :bloom)

(defvar *projects* (au:dict #'eq))

(defclass project ()
  ((%name :reader name
          :initarg :name)
   (%options :reader options
             :initform (au:dict #'eq))))

(defun find-project (name)
  (au:if-let ((project (au:href *projects* name)))
    project
    (error "Project not found: ~a." name)))

(defmacro define-project (name &body body)
  (au:with-unique-names (default user)
    `(progn
       (unless (au:href *projects* ',name)
         (setf (au:href *projects* ',name)
               (make-instance 'project :name ',name)))
       (let ((,default (options (find-project 'bloom)))
             (,user (au:dict #'eq ,@body)))
         (setf (slot-value (find-project ',name) '%options)
               (if ,default
                   (au:merge-tables ,default ,user)
                   ,user))))))

(defun option (project name)
  (let ((project (find-project project)))
    (au:href (options project) name)))

(define-project bloom
  :title "Example Game"
  :window-width 1280
  :window-height 720
  :physics-delta 1/30
  :vsync :on
  :periodic-interval 0.2
  :release nil
  :debug t
  :debug-interval 5
  :log-repl-enable t
  :log-repl-categories '(:bloom)
  :log-file-enable t
  :log-file-error "bloom-error.log"
  :log-file-debug "bloom-debug.log"
  :anti-alias-level 4
  :opengl-version "4.3"
  :default-scene 'default)
