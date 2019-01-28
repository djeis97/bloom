(in-package :bloom)

(defvar *texture-profiles* (au:dict #'eq))
(defvar *texture-definitions* (au:dict #'eq))

;;; Texture profiles

(defclass texture-profile ()
  ((%name :reader name
          :initarg :name)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (au:dict #'eq))))

(defun find-texture-profile (name)
  (au:if-found (profile (au:href *texture-profiles* name))
               profile
               (error "Texture profile ~s does not exist." name)))

(defun update-texture-definitions (profile)
  (au:do-hash (k v (attributes profile))
    (au:do-hash-values (definition *texture-definitions*)
      (when (member (name profile) (overlays definition))
        (setf (au:href (attributes definition) k) v)))))

(defmacro define-texture-profile (name &body body)
  (au:with-unique-names (profile)
    `(let ((,profile (make-instance 'texture-profile :name ',name)))
       (setf ,@(loop :for (attribute value) :on (car body) :by #'cddr
                     :append `((au:href (attributes ,profile) ,attribute) ,value))
             (au:href *texture-profiles* ',name) ,profile)
       (update-texture-definitions ,profile))))

(define-texture-profile default
  (:depth-stencil-texture-mode :depth-component
   :texture-base-level 0
   :texture-border-color (m:get-array (m:vec4))
   :texture-compare-func :lequal
   :texture-compare-mode :none
   :texture-lod-bias 0.0
   :texture-min-filter :nearest-mipmap-linear
   :texture-mag-filter :linear
   :texture-min-lod -1000
   :texture-max-lod 1000
   :texture-max-level 1000
   :texture-swizzle-r :red
   :texture-swizzle-g :green
   :texture-swizzle-b :blue
   :texture-swizzle-a :alpha
   :texture-wrap-s :repeat
   :texture-wrap-t :repeat
   :texture-wrap-r :repeat
   :generate-mipmaps t
   :internal-format :rgb
   :width 0
   :height 0
   :pixel-format :rgb
   :data-type :unsigned-byte
   :data nil))

;;; Texture definitions

(defclass texture-definition ()
  ((%name :reader name
          :initarg :name)
   (%type :reader texture-type
          :initarg :type)
   (%overlays :reader overlays
              :initarg :overlays
              :initform nil)
   (%attributes :accessor attributes
                :initform (au:dict #'eq))))

(defun find-texture-definition (name)
  (au:if-let ((definition (au:href *texture-definitions* name)))
    definition
    (error "Texture ~s does not exist." name)))

(defmethod get-texture-attribute ((object texture-definition) name)
  (au:href (attributes object) name))

(defmacro define-texture (name (&key type overlays) &body body)
  (au:with-unique-names (definition)
    (destructuring-bind (&rest attributes &key data &allow-other-keys) (car body)
      (let ((type (au:format-symbol :keyword "TEXTURE-~:@(~a~)~:[~;-ARRAY~]"
                                    type
                                    (typep data '(cons (cons string *) *))))
            (overlays (remove-duplicates (list* 'default overlays)))
            (attributes (au:plist->hash attributes :test #'eq)))
        `(let ((,definition (make-instance 'texture-definition
                                           :name ',name
                                           :type ,type
                                           :overlays ',overlays)))
           (loop :for profile-name :in ',overlays
                 :for profile = (find-texture-profile profile-name)
                 :do (au:do-hash (k v (attributes profile))
                       (setf (au:href (attributes ,definition) k) v)))
           (setf (attributes ,definition) (au:merge-tables (attributes ,definition) ,attributes)
                 (au:href *texture-definitions* ',name) ,definition))))))

;;; Textures

(au:define-constant +texture-parameters+
    '(:depth-stencil-texture-mode
      :texture-base-level
      :texture-border-color
      :texture-compare-func
      :texture-compare-mode
      :texture-lod-bias
      :texture-min-filter
      :texture-mag-filter
      :texture-min-lod
      :texture-max-lod
      :texture-max-level
      :texture-swizzle-r
      :texture-swizzle-g
      :texture-swizzle-b
      :texture-swizzle-a
      :texture-wrap-s
      :texture-wrap-t
      :texture-wrap-r)
  :test #'equal)

(au:define-constant +sampler-targets+
    (au:dict #'eq
             :sampler-1d :texture-1d
             :isampler-1d :texture-1d
             :usampler-1d :texture-1d
             :sampler-2d :texture-2d
             :isampler-2d :texture-2d
             :usampler-2d :texture-2d
             :sampler-3d :texture-3d
             :isampler-3d :texture-3d
             :usampler-3d :texture-3d
             :sampler-cube :texture-cube-map
             :isampler-cube :texture-cube-map
             :usampler-cube :texture-cube-map
             :sampler-2d-rect :texture-rectangle
             :isampler-2d-rect :texture-rectangle
             :usampler-2d-rect :texture-rectangle
             :sampler-1d-array :texture-1d-array
             :isampler-1d-array :texture-1d-array
             :usampler-1d-array :texture-1d-array
             :sampler-2d-array :texture-2d-array
             :isampler-2d-array :texture-2d-array
             :usampler-2d-array :texture-2d-array
             :sampler-cube-array :texture-cube-map-array
             :isampler-cube-array :texture-cube-map-array
             :usampler-cube-array :texture-cube-map-array
             :sampler-buffer :texture-buffer
             :isampler-buffer :texture-buffer
             :usampler-buffer :texture-buffer
             :sampler-2d-ms :texture-2d-multisample
             :isampler-2d-ms :texture-2d-multisample
             :usampler-2d-ms :texture-2d-multisample
             :sampler-2d-ms-array :texture-2d-multisample-array
             :isampler-2d-ms-array :texture-2d-multisample-array
             :usampler-2d-ms-array :texture-2d-multisample-array)
  :test #'equalp)

(defclass texture ()
  ((%id :reader id
        :initarg :id)
   (%definition :reader definition
                :initarg :definition)))

(defmethod get-texture-attribute ((object texture) name)
  (au:href (attributes (definition object)) name))

(defun set-texture-parameters (texture)
  (let* ((definition (definition texture))
         (target (texture-type definition)))
    (dolist (parameter +texture-parameters+)
      (gl:tex-parameter target parameter (get-texture-attribute texture parameter)))))

(defgeneric %make-texture (type texture data &key &allow-other-keys))

(defmethod %make-texture ((type (eql :texture-2d)) texture data &key)
  (using-image image data
    (gl:tex-image-2d type
                     0
                     (internal-format image)
                     (width image)
                     (height image)
                     0
                     (pixel-format image)
                     (data-type image)
                     (data image))))

(defmethod %make-texture ((type (eql :texture-2d)) texture (data null) &key width height)
  (gl:tex-image-2d type
                   0
                   (get-texture-attribute texture :internal-format)
                   (or width (get-texture-attribute texture :width))
                   (or height (get-texture-attribute texture :height))
                   0
                   (get-texture-attribute texture :pixel-format)
                   (get-texture-attribute texture :data-type)
                   nil))

(defun %load-texture (definition &rest args)
  (unless definition
    (error "Cannot load texture because texture definition named ~s could not be found."
           (name definition)))
  (let* ((id (gl:gen-texture))
         (texture (make-instance 'texture :id id :definition definition))
         (target (texture-type definition)))
    (gl:bind-texture target id)
    (apply #'%make-texture
           target
           texture
           (get-texture-attribute texture :data)
           args)
    (when (get-texture-attribute texture :generate-mipmaps)
      (gl:generate-mipmap target))
    (set-texture-parameters texture)
    (gl:bind-texture target 0)
    id))

(defun load-texture (game-state name &rest args)
  (let* ((definition (au:href *texture-definitions* name))
         (data (get-texture-attribute definition :data)))
    (if data
        (cache-lookup game-state :texture data
          (apply #'%load-texture definition args))
        (apply #'%load-texture definition args))))

;;; Built-in texture definitions

(define-texture-profile framebuffer
  (:texture-min-filter :linear
   :texture-mag-filter :linear
   :data nil))

(define-texture framebuffer-color (:type 2d :overlays (framebuffer)))

(define-texture framebuffer-depth (:type 2d :overlays (framebuffer))
  (:internal-format :depth-component
   :pixel-format :depth-component))

(define-texture framebuffer-stencil (:type 2d :overlays (framebuffer))
  (:internal-format :stencil-index
   :pixel-format :stencil-index))

(define-texture framebuffer-depth/stencil (:type 2d :overlays (framebuffer))
  (:internal-format :depth24-stencil8
   :pixel-format :depth-stencil
   :data-type :unsigned-int-24-8))
