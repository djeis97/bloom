(in-package :bloom)

(defclass frame-manager ()
  ((%start :reader start
           :initform (get-time))
   (%now :initform (get-time))
   (%pause-time :reader pause-time
                :initform 0)
   (%before :initform 0)
   (%total-time :reader total-time
                :initform 0)
   (%delta :reader delta
           :initarg :delta
           :initform (/ 30f0))
   (%delta-buffer :initform 0)
   (%frame-time :reader frame-time
                :initform 0)
   (%frame-count :accessor frame-count
                 :initform 0)
   (%accumulator :initform 0)
   (%factor :reader factor
            :initform 0)
   (%vsync-p :reader vsync-p
             :initarg :vsync-p)
   (%period-elapsed :initform (get-time))
   (%period-interval :reader period-interval
                     :initarg :period
                     :initform nil)
   (%debug-interval :reader debug-interval
                    :initarg :debug-interval
                    :initform 5)
   (%debug-time :initform 0)
   (%debug-count :initform 0)))

(defmethod initialize-instance :after ((object frame-manager) &key)
  (reinitialize-instance object :delta (float (delta object) 1f0)))

(defun make-frame-manager (core)
  (with-slots (%project %frame-manager) core
    (setf %frame-manager
          (make-instance 'frame-manager
                         :vsync-p (option %project :vsync)
                         :delta (option %project :physics-delta)
                         :period (option %project :periodic-interval)
                         :debug-interval (option %project :debug-interval)))))

(defun get-time ()
  #+sbcl
  (multiple-value-bind (s ms) (sb-ext:get-time-of-day)
    (+ (- s (load-time-value (sb-ext:get-time-of-day)))
       (float (/ ms 1e6) 1f0)))
  #-sbcl
  (float (/ (get-internal-real-time) internal-time-units-per-second) 1f0))

(defun smooth-delta-time (frame-manager refresh-rate)
  (with-slots (%delta-buffer %frame-time) frame-manager
    (incf %frame-time %delta-buffer)
    (let* ((frame-count (truncate (1+ (* %frame-time refresh-rate))))
           (previous %frame-time))
      (setf frame-count (if (plusp frame-count) frame-count 1)
            %frame-time (/ frame-count refresh-rate)
            %delta-buffer (- previous %frame-time)))))

(defun calculate-frame-rate (frame-manager)
  (with-slots (%debug-time %debug-count) frame-manager
    (let* ((debug-interval (debug-interval frame-manager))
           (now (get-internal-real-time))
           (elapsed-seconds (/ (- now %debug-time)
                               internal-time-units-per-second))
           (fps (/ %debug-count debug-interval)))
      (when (and (>= elapsed-seconds debug-interval)
                 (plusp fps))
        (v:debug :bloom.frame "Frame rate: ~,2f fps (~,3f ms/f)"
                 fps (/ 1000 fps))
        (setf %debug-count 0
              %debug-time now))
      (incf %debug-count))))

(defun initialize-frame-time (core)
  (with-slots (%start %now) (frame-manager core)
    (let ((time (get-time)))
      (setf %start time
            %now %start))))

(defun perform-physics-update (core)
  (with-slots (%factor %delta %accumulator %frame-time) (frame-manager core)
    (incf %accumulator %frame-time)
    (au:while (>= %accumulator %delta)
      (step/physics core)
      (decf %accumulator %delta))
    (setf %factor (/ %accumulator %delta))))

(defun perform-periodic-update (core)
  (with-slots (%now %period-elapsed %period-interval) (frame-manager core)
    (let ((interval %period-interval))
      (when (and interval
                 (>= (- %now %period-elapsed) interval))
        (step/periodic core)
        (v:trace :bloom.frame
                 "Periodic update performed (every ~d seconds)"
                 interval)
        (setf %period-elapsed %now)))))

(defun tick (core)
  (let ((frame-manager (frame-manager core))
        (refresh-rate (refresh-rate (display core))))
    (with-slots (%start %now %before %total-time %frame-time %pause-time
                 %vsync-p)
        frame-manager
      (setf %before (+ %now %pause-time)
            %now (- (get-time) %pause-time)
            %frame-time (- %now %before)
            %total-time (- %now %start)
            %pause-time 0)
      (when (eq %vsync-p :on)
        (smooth-delta-time frame-manager refresh-rate))
      (perform-physics-update core)
      (perform-periodic-update core)
      (calculate-frame-rate frame-manager)
      (values))))
