(in-package :bloom)

(defclass transform-state ()
  ((%current :accessor current
             :initarg :current)
   (%incremental :accessor incremental
                 :initarg :incremental)
   (%incremental-delta :accessor incremental-delta
                       :initarg :incremental-delta)
   (%previous :accessor previous
              :initarg :previous)
   (%interpolated :accessor interpolated
                  :initarg :interpolated)))

(defclass transform-state-vector (transform-state)
  ()
  (:default-initargs :current (m:vec3)
                     :incremental (m:vec3)
                     :incremental-delta (m:vec3)
                     :previous (m:vec3)
                     :interpolated (m:vec3)))

(defclass transform-state-quaternion (transform-state)
  ()
  (:default-initargs :current (m:quat 1)
                     :incremental (m:quat 1)
                     :incremental-delta (m:quat 1)
                     :previous (m:quat 1)
                     :interpolated (m:quat 1)))

(defun make-translation-state ()
  (make-instance 'transform-state-vector))

(defun make-rotation-state ()
  (make-instance 'transform-state-quaternion
                 :incremental (m:vec3)
                 :incremental-delta (m:vec3)))

(defun make-scaling-state ()
  (make-instance 'transform-state-vector
                 :current (m:vec3 1)))

(defun interpolate-vector (state factor)
  (m:lerp (previous state) (current state) factor (interpolated state)))

(defun interpolate-quaternion (state factor)
  (m:slerp (previous state) (current state) factor (interpolated state)))

(define-component transform ()
  (parent nil)
  (children nil)
  (translation (make-translation-state))
  (rotation (make-rotation-state))
  (scaling (make-scaling-state))
  (local (m:mat4 1))
  (model (m:mat4 1)))

(defun add-child (parent child)
  (pushnew child (children parent))
  (setf (parent child) parent))

(defun remove-child (parent child)
  (setf (children parent) (remove-if (lambda (x) (eq x child)) (children parent))
        (parent child) nil))

(defun transform-node (game-state node)
  (let ((delta (delta (frame-manager game-state))))
    (with-slots (%previous %current %incremental-delta %incremental) (scaling node)
      (m:copy-into %previous %current)
      (m:+ %current (m:* %incremental delta %incremental-delta) %current))
    (with-slots (%previous %current %incremental-delta %incremental) (rotation node)
      (m:copy-into %previous %current)
      (m:rotate :local %current (m:* %incremental delta %incremental-delta) %current))
    (with-slots (%previous %current %incremental-delta %incremental) (translation node)
      (m:copy-into %previous %current)
      (m:+ %current (m:* %incremental delta %incremental-delta) %current))))

(defun resolve-local (node alpha)
  (with-slots (%local %scaling %rotation %translation) node
    (interpolate-vector %scaling alpha)
    (interpolate-quaternion %rotation alpha)
    (interpolate-vector %translation alpha)
    (m:* (m:copy-into %local (m:mat4 (interpolated %rotation)))
         (m:set-scale m:+id-mat4+ (interpolated %scaling))
         %local)
    (m:set-translation %local (interpolated %translation) %local)))

(defun resolve-model (node alpha)
  (au:when-let ((parent (parent node)))
    (resolve-local node alpha)
    (m:* (model parent) (local node) (model node))))

(defun map-nodes (func parent)
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

(defun interpolate-transforms (game-state)
  (with-slots (%active-scene %frame-manager) game-state
    (map-nodes
     (lambda (node)
       (resolve-model node (alpha %frame-manager)))
     (root-node %active-scene))))

(defmethod reinitialize-instance ((instance transform)
                                  &key
                                    id
                                    (translate (m:vec3))
                                    (translate/inc (m:vec3))
                                    (rotate (m:vec3))
                                    (rotate/inc (m:vec3))
                                    (scale (m:vec3 1))
                                    (scale/inc (m:vec3)))
  (with-slots (%translation %rotation %scaling) instance
    (setf (component-id instance) id
          (current %translation) translate
          (previous %translation) (m:copy (current %translation))
          (incremental %translation) translate/inc
          (current %rotation) (etypecase rotate
                                (m:vec3 (m:rotate :local m:+id-quat+ rotate))
                                (m:quat rotate))
          (previous %rotation) (current %rotation)
          (incremental %rotation) rotate/inc
          (current %scaling) (etypecase scale
                               (m:vec3 scale)
                               (real (m:vec3 scale)))
          (previous %scaling) (m:copy (current %scaling))
          (incremental %scaling) scale/inc)))

;;; User API

(defun translate-transform (transform vec &key replace-p instant-p)
  (with-slots (%current %previous) (translation transform)
    (m:+ (if replace-p m:+zero-vec3+ %current) vec %current)
    (when instant-p
      (m:copy-into %previous %current))))

(defun rotate-transform (transform vec &key replace-p instant-p)
  (with-slots (%current %previous) (rotation transform)
    (m:rotate :local (if replace-p m:+id-quat+ %current) vec %current)
    (when instant-p
      (m:copy-into %previous %current))))

(defun scale-transform (transform vec &key replace-p instant-p)
  (with-slots (%current %previous) (scaling transform)
    (m:+ (if replace-p m:+zero-vec3+ %current) vec %current)
    (when instant-p
      (m:copy-into %previous %current))))

;;; Component event hooks

(defmethod on-component-update ((self transform))
  (transform-node (game-state self) self))
