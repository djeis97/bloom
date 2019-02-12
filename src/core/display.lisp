(in-package :bloom)

(defclass display ()
  ((%game-state :reader game-state
                :initarg :game-state)
   (%window :reader window
            :initarg :window)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)))

(defun parse-opengl-version-string (string)
  (values-list (mapcar #'parse-integer (au:split-sequence #\. string))))

(defun maybe-set-vsync (value)
  (let ((value
          (ecase value
            (:on 1)
            (:off 0)
            (:adaptive -1))))
    (labels ((try (current-value)
               (handler-case (sdl2:gl-set-swap-interval current-value)
                 (sdl2::sdl-rc-error ()
                   (if (= current-value -1)
                       (try 1)
                       (v:warn
                        :bloom.display.vsync
                        "Ignoring vsync option due to driver limitation."))))))
      (try value))))

(defgeneric create-window (game-state)
  (:method :before (game-state)
    (let* ((project (project game-state))
           (opengl-version (option project :opengl-version))
           (anti-alias-level (option project :anti-alias-level)))
      (au:mvlet ((major-version minor-version (parse-opengl-version-string
                                               opengl-version)))
        (sdl2:gl-set-attrs :context-major-version major-version
                           :context-minor-version minor-version
                           :context-profile-mask 1
                           :multisamplebuffers (if (zerop anti-alias-level) 0 1)
                           :multisamplesamples anti-alias-level))))
  (:method (game-state)
    (let* ((project (project game-state))
           (window (sdl2:create-window :title (option project :title)
                                       :w (option project :window-width)
                                       :h (option project :window-height)
                                       :flags '(:opengl))))
      (sdl2:gl-create-context window)
      window)))

(defmethod initialize-instance :after ((instance display) &key &allow-other-keys)
  (with-slots (%display %project) (game-state instance)
    (setf %display instance)
    (apply #'gl:enable '(:depth-test :blend :multisample :cull-face))
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (maybe-set-vsync (option %project :vsync))))

(defun make-display (game-state)
  (let ((window (create-window game-state))
        (refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0))))
    (make-instance 'display :game-state game-state
                            :window window
                            :refresh-rate refresh-rate)))

(defun clear-screen (display)
  (with-slots (%project %frame-manager) (game-state display)
    (multiple-value-call #'gl:clear-color
      (if (option %project :debug)
          (values (* 0.25 (abs (sin (total-time %frame-manager)))) 0 0 1)
          (values 0 0 0 1)))
    (gl:clear :color-buffer :depth-buffer)))
