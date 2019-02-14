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
   (%alpha :reader alpha
           :initform 0f0)
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

(defun get-time ()
  #+sbcl
  (multiple-value-bind (s ms) (sb-ext:get-time-of-day)
    (+ (- s (load-time-value (sb-ext:get-time-of-day)))
       (float (/ ms 1e6) 1d0)))
  #-sbcl
  (float (/ (get-internal-real-time) internal-time-units-per-second) 1d0))

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
        (v:debug :bloom.engine.frame.rate "Frame rate: ~,2f fps (~,3f ms/f)"
                 fps (/ 1000 fps))
        (setf %debug-count 0
              %debug-time now))
      (incf %debug-count))))

(defun initialize-frame-time (game-state)
  (with-slots (%start %now) (frame-manager game-state)
    (let ((time (get-time)))
      (setf %start time
            %now %start))))

(defun perform-physics-update (game-state)
  (with-slots (%alpha %delta %accumulator %frame-time) (frame-manager game-state)
    (incf %accumulator %frame-time)
    (au:while (>= %accumulator %delta)
      (map-component-type game-state 'transform #'on-component-update)
      (map-components game-state #'on-component-physics-update)
      (decf %accumulator %delta))
    (setf %alpha (/ %accumulator %delta))))

(defun perform-periodic-update (game-state)
  (with-slots (%now %period-elapsed %period-interval) (frame-manager game-state)
    (let ((interval %period-interval))
      (when (and interval
                 (>= (- %now %period-elapsed) interval))
        (periodic-update-step game-state)
        (v:trace :bloom.engine.frame.periodic-update
                 "Periodic update performed (every ~d seconds)"
                 interval)
        (setf %period-elapsed %now)))))

(defun tick (game-state)
  (let ((frame-manager (frame-manager game-state))
        (refresh-rate (refresh-rate (display game-state))))
    (with-slots (%start %now %before %total-time %frame-time %pause-time
                 %vsync-p)
        frame-manager
      (setf %before (+ %now %pause-time)
            %now (- (get-time) %pause-time)
            %frame-time (float (- %now %before) 1d0)
            %total-time (float (- %now %start) 1d0)
            %pause-time 0)
      (when %vsync-p
        (smooth-delta-time frame-manager refresh-rate))
      (perform-physics-update game-state)
      (perform-periodic-update game-state)
      (calculate-frame-rate frame-manager)
      (values))))
