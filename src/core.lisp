(in-package :bloom)

(defvar *core* nil)

(defclass core ()
  ((%project :reader project
             :initarg :project)
   (%running-p :accessor running-p
               :initform t)
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
   (%resource-cache :reader resource-cache
                    :initform (au:dict #'equalp))
   (%storage :reader storage
             :initform (au:dict #'eq))
   (%scenes :reader scenes
            :initform (make-instance 'scene-manager))))

(defun launch (project &optional duration)
  (unless *core*
    (unwind-protect
         (let ((core (make-instance 'core :project project)))
           (setf *core* core)
           (step/start core)
           (if duration
               (with-profile (main-loop core duration))
               (main-loop core)))
      (force-quit))))

(defun stop (core)
  (let ((title (option (project core) :title)))
    (v:info :bloom.core "Shutting down ~a..." title)
    (shutdown-host core)
    (setf (running-p core) nil
          *core* nil)
    (v:info :bloom.core "~a successfully exited." title)))

(defun force-quit ()
  (when *core*
    (shutdown-host *core*)
    (setf *core* nil)))

(defun step/start (core)
  (let ((title (option (project core) :title)))
    (v:info :bloom.core "Starting ~a..." title)
    (setup-live-coding)
    (enable-logging core)
    (compute-component-type-order core)
    (make-frame-manager core)
    (initialize-host core)
    (initialize-shaders core)
    (initialize-framebuffers core)
    (make-scenes core)
    (initialize-frame-time core)
    (v:info :bloom.core "~a is now running." title)))

(defun step/periodic (core)
  (live-coding-update)
  (process-tasks core))

(defun step/physics (core)
  (map-component-type core 'transform #'on-component-update)
  (flow/actions core)
  (map-components core #'on-component-physics-update))

(defun step/frame (core)
  (with-continue-restart "Bloom"
    (with-slots (%running-p %display %frame-manager) core
      (when %running-p
        (tick core)
        (handle-events core)
        (flow/update core)
        (interpolate-transforms core)
        (map-components core #'on-component-update)
        (update-display core))
      ;; TODO: Remove this later when possible.
      (when (input-enter-p core '(:key :escape))
        (stop core)))))

(defun main-loop (core &optional duration)
  (au:while (running-p core)
    (when (and duration
               (>= (total-time (frame-manager core)) duration))
      (stop core))
    (step/frame core)))
