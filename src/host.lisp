(in-package :bloom)

(defun initialize-host (core)
  (let ((flags '(:everything)))
    (unless (apply #'sdl2:was-init flags)
      (let ((flags (autowrap:mask-apply 'sdl2::sdl-init-flags flags)))
        (sdl2::check-rc (sdl2::sdl-init flags))))
    (prepare-gamepads)
    (make-display core)))

(defun shutdown-host (core)
  (shutdown-gamepads)
  (sdl2:destroy-window (window (display core)))
  (sdl2::sdl-quit))
