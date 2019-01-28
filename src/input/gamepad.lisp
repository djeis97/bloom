(in-package :bloom)

(au:define-constant +gamepad-axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y) (:triggers :x)
      (:triggers :y))
  :test #'equalp)

(au:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick-button :right-stick-button :left-shoulder
      :right-shoulder :up :down :left :right)
  :test #'equalp)

(defclass gamepad-manager ()
  ((%instances :reader instances
               :initform (au:dict #'eq))
   (%ids :reader ids
         :initform (au:dict #'eq))
   (%detached :accessor detached
              :initform nil)))

(defclass gamepad ()
  ((%id :reader id
        :initarg :id)
   (%instance :reader instance
              :initarg :instance)
   (%name :reader name
          :initarg :name)
   (%handle :reader handle
            :initarg :handle)))

(defclass gamepad-analog-state ()
  ((%x :accessor x
       :initform 0.0)
   (%y :accessor y
       :initform 0.0)
   (%deadzone :accessor deadzone
              :initform 0.0)))

(defvar *gamepad-manager* (make-instance 'gamepad-manager))

;;; Utility functions

(defun get-gamepad-by-instance (gamepad-instance)
  (au:href (instances *gamepad-manager*) gamepad-instance))

(defun generate-gamepad-id ()
  (let ((instance-count (hash-table-count (instances *gamepad-manager*))))
    (or (pop (detached *gamepad-manager*))
        (au:format-symbol :keyword "GAMEPAD~d" (1+ instance-count)))))

(defun normalize-gamepad-analog-value (sub-device axis value)
  (if (eq sub-device :triggers)
      (au:map-domain 0 32767 0 1 value)
      (let ((clamped (au:clamp value -32767 32767)))
        (case axis
          (:x (au:map-domain -32767 32767 -1 1 clamped))
          (:y (au:map-domain -32767 32767 1 -1 clamped))))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :axial)) analog-state)
  (let ((x (x analog-state))
        (y (y analog-state)))
    (m:with-vec2 ((v (m:vec2 x y)))
      (m:stabilize v (deadzone analog-state) v)
      (values v.x v.y))))


(defmethod %get-gamepad-analog ((deadzone-type (eql :radial)) analog-state)
  (let ((x (x analog-state))
        (y (y analog-state)))
    (m:with-vec2 ((v (m:vec2 x y)))
      (if (< (m:length v) (deadzone analog-state))
          (values 0f0 0f0)
          (values v.x v.y)))))

(defmethod %get-gamepad-analog ((deadzone-type (eql :radial-scaled)) analog-state)
  (let ((x (x analog-state))
        (y (y analog-state))
        (deadzone (deadzone analog-state)))
    (m:with-vec2 ((v (m:vec2 x y)))
      (let ((length (m:length v)))
        (if (< length deadzone)
            (values 0f0 0f0)
            (progn
              (m:* (m:normalize v) (/ (- length deadzone) (- 1 deadzone)) v)
              (values v.x v.y)))))))

(defun load-gamepad-database ()
  (sdl2:game-controller-add-mappings-from-file
   (namestring
    (uiop/pathname:merge-pathnames*
     (resolve-path :misc "gamepads.txt")))))

(defun enable-background-gamepad-events ()
  (sdl2-ffi.functions:sdl-set-hint sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1"))

(defun prepare-gamepads ()
  (load-gamepad-database)
  (enable-background-gamepad-events))

(defun shutdown-gamepads ()
  (let ((instance-table (instances *gamepad-manager*)))
    (au:do-hash-values (v instance-table)
      (sdl2:game-controller-close (handle v)))
    (clrhash instance-table)))

;;; Events

(defun on-gamepad-attach (input-data gamepad-index)
  (when (sdl2:game-controller-p gamepad-index)
    (let* ((handle (sdl2:game-controller-open gamepad-index))
           (instance (sdl2:game-controller-instance-id handle))
           (id (generate-gamepad-id))
           (gamepad (make-instance 'gamepad :id id
                                            :instance instance
                                            :name (sdl2:game-controller-name handle)
                                            :handle handle)))
      (setf (au:href (instances *gamepad-manager*) instance) gamepad
            (au:href (ids *gamepad-manager*) id) gamepad)
      (input-transition-in input-data (list id :attach)))))

(defun on-gamepad-detach (input-data gamepad-instance)
  (let* ((instance-table (instances *gamepad-manager*))
         (id-table (ids *gamepad-manager*))
         (gamepad (au:href instance-table gamepad-instance))
         (id (id gamepad)))
    (sdl2:game-controller-close (handle gamepad))
    (au:appendf (detached *gamepad-manager*) (list id))
    (remhash id id-table)
    (remhash gamepad-instance instance-table)
    (input-transition-out input-data (list id :attach))))

(defun on-gamepad-analog-move (input-data gamepad-instance axis value)
  (destructuring-bind (sub-device axis) axis
    (let* ((gamepad (get-gamepad-by-instance gamepad-instance))
           (key (list (id gamepad) sub-device))
           (value (normalize-gamepad-analog-value sub-device axis value)))
      (symbol-macrolet ((state (au:href (states input-data) key)))
        (if (not state)
            (setf state (make-instance 'gamepad-analog-state))
            (case axis
              (:x (setf (x state) value))
              (:y (setf (y state) value))))))))

(defun on-gamepad-button-up (input-data gamepad-instance button)
  (let* ((gamepad (get-gamepad-by-instance gamepad-instance))
         (id (id gamepad)))
    (input-transition-out input-data (list id button))
    (input-transition-out input-data (list id :any))
    (input-transition-out input-data '(:button :any))))

(defun on-gamepad-button-down (input-data gamepad-instance button)
  (let* ((gamepad (get-gamepad-by-instance gamepad-instance))
         (id (id gamepad)))
    (input-transition-in input-data (list id button))
    (input-transition-in input-data (list id :any))
    (input-transition-in input-data '(:button :any))))

;;; User protocol

(defun get-gamepad-name (gamepad-id)
  (let ((gamepad (au:href (ids *gamepad-manager*) gamepad-id)))
    (name gamepad)))

(defun get-gamepad-analog (input-data input)
  (au:if-found (state (au:href (states input-data) input))
               (%get-gamepad-analog :radial-scaled state)
               (values 0f0 0f0)))
