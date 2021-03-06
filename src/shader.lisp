(in-package :bloom)

(defun initialize-shaders (core)
  (shadow:reset-program-state)
  (shadow:enable-dependency-tracking)
  (shadow:build-shader-dictionary)
  (shadow:set-modify-hook (generate-shader-modified-hook core)))

(defmethod perform-task (core (type (eql :shader)) data)
  (when data
    (shadow:translate-shader-programs data)
    (shadow:build-shader-programs data)
    (shadow:rebind-blocks data)
    (shader-modified-post-hook core)
    (v:debug :bloom.shader "Recompiled shader programs: ~{~s~^, ~}" data)))

(defun generate-shader-modified-hook (core)
  (lambda (data) (schedule-task core :shader data)))

(defmethod shader-modified-post-hook (core))

(defun shutdown-shaders ()
  (shadow:set-modify-hook (constantly nil))
  (shadow:disable-dependency-tracking))

(defun make-shader-blocks (core shaders name)
  (symbol-macrolet ((binding (au:href (storage core) 'shader-bindings)))
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
