(in-package :bloom)

(defvar *options* (au:dict #'eq))

(defmacro define-options ((project) &body body)
  (au:with-unique-names (default user)
    `(let ((,default (au:href *options* 'default))
           (,user (au:dict #'eq ,@body)))
       (setf (au:href *options* ',project)
             (if ,default
                 (au:merge-tables ,default ,user)
                 ,user)))))

(define-options (default)
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

(defun option (project name)
  (au:if-found (options (au:href *options* project))
               (au:href options name)
               (au:href *options* 'default name)))
