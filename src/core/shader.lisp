(in-package :bloom)

(defvar *shader-definitions* (au:dict #'eq))

(defmacro define-shader (name (&key (version :430) (primitive :triangles)) &body body)
  (let ((shader-name (au:format-symbol :bloom.shader "~a" name)))
    `(progn
       (setf (au:href *shader-definitions* ',shader-name)
             (lambda ()
               (let ((*package* (find-package :bloom.shader)))
                 (shadow:define-shader ,shader-name (:version ,version :primitive ,primitive)
                   ,@body))))
       (export ',shader-name))))

(defmacro define-shader-struct (name () &body slots)
  `(shadow:define-gpu-struct ,name () ,@slots))

(defmacro define-shader-function (name args &body body)
  `(shadow:define-gpu-function ,name ,args ,@body))

(defun initialize-shaders (game-state)
  (shadow:reset-program-state)
  (au:do-hash-values (shader-factory *shader-definitions*)
    (funcall shader-factory))
  (shadow:enable-dependency-tracking)
  (shadow:build-shader-dictionary)
  (shadow:set-modify-hook (generate-shader-modified-hook game-state)))

(defmethod perform-task (game-state (type (eql :shader)) data)
  (when data
    (shadow:translate-shader-programs data)
    (shadow:build-shader-programs data)
    (shadow:rebind-blocks data)
    (shader-modified-post-hook game-state)
    (v:debug :bloom.shader.recompile "Recompiled shader programs: ~{~s~^, ~}" data)))

(defun generate-shader-modified-hook (game-state)
  (lambda (data) (schedule-task game-state :shader data)))

(defmethod shader-modified-post-hook (game-state))

(defun shutdown-shaders ()
  (shadow:set-modify-hook (constantly nil))
  (shadow:disable-dependency-tracking))

(defun make-shader-blocks (game-state shaders name)
  (symbol-macrolet ((binding (au:href (shared-storage game-state) :shader-bindings)))
    (unless binding
      (setf binding 0))
    (incf binding)
    (loop :for shader :in shaders
          :for alias = (au:format-symbol :keyword "~a/~a" shader name)
          :do (shadow:create-block-alias :buffer name shader alias)
              (shadow:bind-block alias binding)
          :collect alias :into aliases
          :finally (return (values aliases binding)))))

(defun make-shader-buffer (buffer-name block-name binding object)
  (unless (shadow:find-buffer buffer-name)
    (shadow:create-buffer buffer-name block-name)
    (shadow:bind-buffer buffer-name binding)
    (write-shader-buffer buffer-name object)))
