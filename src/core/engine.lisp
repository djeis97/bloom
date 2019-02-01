(in-package :bloom)

(defvar *game-state*)

(defclass game-state ()
  ((%running-p :accessor running-p
               :initform t)
   (%options :accessor options)
   (%frame-manager :accessor frame-manager)
   (%display :accessor display)
   (%input-data :accessor input-data
                :initform (make-instance 'input-data))
   (%tasks :reader tasks
           :initform (queues:make-queue :simple-cqueue))
   (%component-data :reader component-data
                    :initform (make-instance 'component-data))
   (%framebuffers :reader framebuffers
                  :initform (au:dict #'eq))
   (%viewport-mode :accessor viewport-mode
                   :initform :full-screen)
   (%resource-cache :reader resource-cache
                    :initform (au:dict #'equalp))
   (%shared-storage :reader shared-storage
                    :initform (au:dict #'eq))
   (%scenes :reader scenes
            :initform (au:dict #'eq))
   (%active-scene :accessor active-scene)
   (%next-scene :accessor next-scene
                :initform nil)))

(defun make-frame-manager (game-state)
  (setf (frame-manager game-state)
        (make-instance 'frame-manager
                       :vsync-p (option game-state :vsync)
                       :delta (option game-state :physics-delta)
                       :period (option game-state :periodic-interval)
                       :debug-interval (option game-state :debug-interval))))

(defun update-step (game-state)
  (with-slots (%component-data %active-scene) game-state
    (when (cache-dirty-p %component-data)
      (cache-transform-components game-state))
    (map-components game-state #'on-component-update)
    (au:do-hash-values (entity (au:href (entities %active-scene) :active-by-name))
      (process-actions (actions entity)))))

(defun periodic-update-step (game-state)
  (update-lisp-repl)
  (process-tasks game-state))

(defun render-step (game-state)
  (with-slots (%running-p %display %frame-manager) game-state
    (when %running-p
      (clear-screen %display)
      (map-components game-state #'on-component-render)
      (sdl2:gl-swap-window (window %display))
      (incf (frame-count %frame-manager)))))

(defun step-frame (game-state)
  (with-continue-restart "Bloom"
    (flow/update game-state)
    (handle-events (input-data game-state))
    (tick game-state)
    (interpolate-transforms game-state)
    (render-step game-state)
    ;; TODO: Remove this later when possible.
    (when (input-enter-p game-state '(:key :escape))
      (stop-engine game-state))))

(defun main-loop (game-state)
  (initialize-frame-time game-state)
  (au:while (running-p game-state)
    (step-frame game-state)))

(defun initialize-engine (game-state)
  (setup-lisp-repl)
  (load-options game-state)
  (let ((title (option game-state :title)))
    (v:info :bloom.engine.start "Starting ~a..." title)
    (enable-logging game-state)
    (compute-component-type-order game-state)
    (make-frame-manager game-state)
    (initialize-host game-state)
    (initialize-shaders game-state)
    (initialize-framebuffers game-state)
    (make-scenes game-state)
    (v:info :bloom.engine.start "~a is now running." title)))

(defun start-engine (&key profile-duration)
  (unwind-protect
       (let ((game-state (make-instance 'game-state)))
         (setf *game-state* game-state)
         (initialize-engine game-state)
         (if profile-duration
             (profile game-state profile-duration)
             (main-loop game-state)))
    (force-quit)))

(defun stop-engine (game-state)
  (let ((title (option game-state :title)))
    (v:info :bloom.engine.stop "Shutting down ~a..." title)
    (shutdown-host game-state)
    (setf (running-p game-state) nil
          *game-state* nil)
    (v:info :bloom.engine.stop "~a successfully exited." title)))

(defun force-quit ()
  (when *game-state*
    (shutdown-host *game-state*)
    (setf *game-state* nil)))

(defun profile (game-state duration)
  (with-profile
    (let ((frame-manager (frame-manager game-state)))
      (au:while (and (running-p game-state)
                     (<= (total-time frame-manager) duration))
        (step-frame game-state))
      (when (running-p game-state)
        (stop-engine game-state)))))
