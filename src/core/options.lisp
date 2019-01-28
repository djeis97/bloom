(in-package :bloom)

(defvar *default-options*
  (au:dict :application-name 'bloom
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
           :default-scene 'default))

(defvar *user-options* (au:dict))

(defun load-options (game-state)
  (setf (options game-state) (au:merge-tables *default-options* *user-options*)))

(defmacro define-options (() &body body)
  `(setf *user-options* (au:dict ,@body)))

(defun option (game-state option-name)
  (au:href (options game-state) option-name))
