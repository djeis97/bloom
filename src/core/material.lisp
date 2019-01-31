(in-package :bloom)

(defvar *material-definitions* (au:dict #'eq))

(defclass material-definition ()
  ((%id :reader id
        :initarg :id)
   (%shader :reader shader
            :initarg :shader)
   (%target :reader target
            :initarg :target)
   (%uniforms :reader uniforms
              :initarg :uniforms)))

(defun find-material-definition (id)
  (au:href *material-definitions* id))

(defun make-material-definition-uniform-table (uniforms-spec)
  (au:merge-tables
   (au:dict #'eq
            :model (lambda (x) (get-render-model x))
            :view (lambda (x) (get-render-view x))
            :proj (lambda (x) (get-render-projection x))
            :time (lambda (x) (get-render-time x)))
   (apply #'au:dict #'eq uniforms-spec)))

(defun copy-material-definition-uniforms (material-definition)
  (let ((uniforms (au:dict #'eq)))
    (labels ((copy (value)
               (if (typep value 'sequence)
                   (map-into (copy-seq value) #'copy value)
                   value)))
      (when material-definition
        (au:do-hash (k v (uniforms material-definition))
          (setf (au:href uniforms k) (copy v))))
      uniforms)))

(defmacro define-material (id (&optional base) &body body)
  (au:with-unique-names (base-material base-uniforms uniforms-table)
    (destructuring-bind (&key shader target uniforms) (car body)
      `(let* ((,uniforms-table (make-material-definition-uniform-table ,uniforms))
              (,base-material (find-material-definition ',base))
              (,base-uniforms (copy-material-definition-uniforms ,base-material)))
         (setf (au:href *material-definitions* ',id)
               (make-instance 'material-definition
                              :id ',id
                              :shader (or ',shader (and ,base-material (shader ,base-material)))
                              :target (or ',target (and ,base-material (target ,base-material)))
                              :uniforms (au:merge-tables ,base-uniforms ,uniforms-table)))))))

(defclass material ()
  ((%id :reader id
        :initarg :id)
   (%renderer :reader renderer
              :initarg :renderer)
   (%framebuffer :reader framebuffer
                 :initarg :framebuffer)
   (%attachments :reader attachments
                 :initarg :attachments)
   (%clear-buffers :reader clear-buffers
                   :initarg :clear-buffers)
   (%shader :reader shader
            :initarg :shader)
   (%uniforms :reader uniforms
              :initarg :uniforms)
   (%texture-unit-state :accessor texture-unit-state
                        :initform 0)))

(defclass material-uniform ()
  ((%name :reader name
          :initarg :name)
   (%type :reader uniform-type
          :initarg :type)
   (%value :accessor value
           :initarg :value)
   (%actual-value :accessor actual-value
                  :initform :unset)
   (%binder :reader binder
            :initarg :binder)))

(defun make-material (definition renderer)
  (destructuring-bind (&key framebuffer attachments clear-buffers) (target definition)
    (let* ((game-state (game-state renderer))
           (shader (or (shader renderer) (shader definition)))
           (framebuffer (find-framebuffer game-state framebuffer))
           (attachments (framebuffer-attachment-names->points framebuffer attachments))
           (program (shadow:find-program shader))
           (uniforms (au:plist->hash (uniforms renderer) :test #'eq))
           (material (make-instance 'material
                                    :id (id definition)
                                    :renderer renderer
                                    :framebuffer framebuffer
                                    :attachments attachments
                                    :clear-buffers clear-buffers
                                    :shader shader
                                    :uniforms uniforms)))
      (au:do-hash (k v (uniforms definition))
        (when (au:href (shadow:uniforms program) k)
          (symbol-macrolet ((uniform (au:href uniforms k)))
            (setf uniform (make-material-uniform material k (or uniform v))))))
      material)))

(defun make-material-uniform (material name value)
  (with-slots (%renderer %shader) material
    (let* ((program (shadow:find-program %shader))
           (type (au:href (shadow:uniforms program) name :type))
           (resolved-type (resolve-uniform-type type)))
      (make-instance 'material-uniform
                     :name name
                     :type type
                     :value (transform-uniform (game-state %renderer) value resolved-type)
                     :binder (generate-uniform-binder material type)))))

(defun resolve-uniform-type (uniform-type)
  (if (search "SAMPLER" (symbol-name uniform-type))
      :sampler
      uniform-type))

(defun resolve-material (material renderer)
  (au:do-hash-values (uniform (uniforms material))
    (with-slots (%value %actual-value) uniform
      (setf %actual-value
            (typecase %value
              ((or function symbol)
               (funcall %value renderer))
              (t
               %value))))))

(defun generate-uniform-binder (material uniform-type)
  (etypecase uniform-type
    (symbol
     (ecase (resolve-uniform-type uniform-type)
       (:sampler
        (let ((unit (texture-unit-state material)))
          (incf (texture-unit-state material))
          (lambda (shader uniform value)
            (gl:active-texture unit)
            (gl:bind-texture (au:href +sampler-targets+ uniform-type) value)
            (shadow:uniform-int shader uniform unit))))
       ((:bool :int) #'shadow:uniform-int)
       (:float #'shadow:uniform-float)
       (:vec2 (lambda (shader uniform value)
                (shadow:uniform-vec2 shader uniform (m:get-array value))))
       (:vec3 (lambda (shader uniform value)
                (shadow:uniform-vec3 shader uniform (m:get-array value))))
       (:vec4 (lambda (shader uniform value)
                (shadow:uniform-vec4 shader uniform (m:get-array value))))
       (:mat2 (lambda (shader uniform value)
                (shadow:uniform-mat2 shader uniform (m:get-array value))))
       (:mat3 (lambda (shader uniform value)
                (shadow:uniform-mat3 shader uniform (m:get-array value))))
       (:mat4 (lambda (shader uniform value)
                (shadow:uniform-mat4 shader uniform (m:get-array value))))))
    (cons
     (destructuring-bind (uniform-type . dimensions) uniform-type
       (ecase (resolve-uniform-type uniform-type)
         (:sampler
          (let* ((unit-list (au:iota dimensions :start (texture-unit-state material)))
                 (unit-array (make-array dimensions :initial-contents unit-list)))
            (incf (texture-unit-state material) dimensions)
            (lambda (shader uniform value)
              (loop :for texture :in value
                    :for unit :in unit-list
                    :do (gl:active-texture unit)
                        (gl:bind-texture (au:href +sampler-targets+ uniform-type) texture))
              (shadow:uniform-int-array shader uniform unit-array))))
         ((:bool :int) #'shadow:uniform-int-array)
         (:float #'shadow:uniform-float-array))))))

(defgeneric transform-uniform (game-state uniform-value type)
  (:method (game-state uniform-value type)
    uniform-value)
  (:method (game-state (uniform-value symbol) (type (eql :sampler)))
    (load-texture game-state uniform-value))
  (:method (game-state (uniform-value list) (type (eql :sampler)))
    (destructuring-bind (&key framebuffer attachment) uniform-value
      (find-framebuffer-texture-id game-state framebuffer attachment))))

(defmacro with-material ((material) &body body)
  `(shadow:with-shader-program (shader ,material)
     (au:do-hash (name uniform (uniforms ,material))
       (funcall (binder uniform) (shader ,material) name (actual-value uniform)))
     ,@body))
