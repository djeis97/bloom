(in-package :bloom)

(defclass transform-state ()
  ((%current :accessor current
             :initarg :current)
   (%frame :accessor frame
           :initarg :frame)
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
                     :frame (m:vec3)
                     :incremental (m:vec3)
                     :incremental-delta (m:vec3)
                     :previous (m:vec3)
                     :interpolated (m:vec3)))

(defclass transform-state-quaternion (transform-state)
  ()
  (:default-initargs :current (m:quat 1)
                     :frame (m:vec3)
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

(defun transform-node/vector (state delta frame-time)
  (with-slots (%frame %previous %current %incremental-delta %incremental) state
    (m:copy-into %previous %current)
    (m:* %frame frame-time %frame)
    (m:+ %current %frame %current)
    (m:* %incremental delta %incremental-delta)
    (m:+ %current %incremental-delta %current)
    (m:zero %frame)))

(defun transform-node/quat (state delta frame-time)
  (with-slots (%frame %previous %current %incremental-delta %incremental) state
    (m:copy-into %previous %current)
    (m:* %frame frame-time %frame)
    (m:rotate :local %current %frame %current)
    (m:* %incremental delta %incremental-delta)
    (m:rotate :local %current %incremental-delta %current)
    (m:zero %frame)))

(defun transform-node (core node)
  (with-slots (%delta %frame-time) (frame-manager core)
    (transform-node/vector (scaling node) %delta %frame-time)
    (transform-node/quat (rotation node) %delta %frame-time)
    (transform-node/vector (translation node) %delta %frame-time)))

(defun resolve-local (node factor)
  (with-slots (%local %scaling %rotation %translation) node
    (interpolate-vector %scaling factor)
    (interpolate-quaternion %rotation factor)
    (interpolate-vector %translation factor)
    (m:* (m:copy-into %local (m:mat4 (interpolated %rotation)))
         (m:set-scale m:+id-mat4+ (interpolated %scaling))
         %local)
    (m:set-translation %local (interpolated %translation) %local)))

(defun resolve-model (node factor)
  (au:when-let ((parent (parent node)))
    (resolve-local node factor)
    (m:* (model parent) (local node) (model node))))

(defun map-nodes (func parent)
  (funcall func parent)
  (dolist (child (children parent))
    (map-nodes func child)))

(defun interpolate-transforms (core)
  (map-nodes
   (lambda (node)
     (resolve-model node (factor (frame-manager core))))
   (root-node (get-current-scene core))))

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

(defun prune-tree (transform)
  (au:when-let* ((parent-transform (parent transform))
                 (parent (entity parent-transform)))
    (au:deletef (children (get-entity-component parent 'transform)) transform))
  (map-nodes
   (lambda (node)
     (let ((entity (entity node)))
       (setf (state entity) :destroy)))
   transform))

;;; User API

(defun translate-transform (transform vec)
  (with-slots (%frame) (translation transform)
    (m:+ %frame vec %frame)))

(defun rotate-transform (transform vec)
  (with-slots (%frame) (rotation transform)
    (m:+ %frame vec %frame)))

(defun scale-transform (transform vec)
  (with-slots (%frame) (scaling transform)
    (m:+ %frame vec %frame)))

(defun get-translation (transform)
  (current (translation transform)))

(defun get-rotation (transform)
  (current (rotation transform)))

(defun get-scale (transform)
  (current (rotation transform)))

;;; Component event hooks

(defmethod on-component-update ((self transform))
  (transform-node (core self) self))
