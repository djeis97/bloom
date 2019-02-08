(in-package :bloom)

(defvar *framebuffer-definitions* (au:dict #'eq))

;;; Framebuffer definitions

(defclass framebuffer-definition ()
  ((%name :reader name
          :initarg :name)
   (%mode :reader mode
          :initarg :mode)
   (%attachments :reader attachments
                 :initarg :attachments
                 :initform (au:dict #'eq))))

(defclass framebuffer-attachment-definition ()
  ((%name :reader name
          :initarg :name)
   (%type :reader attachment-type
          :initarg :type)
   (%point :reader point
           :initarg :point)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)))

(defun make-framebuffer-attachment-definition (spec)
  (flet ((generate-size-func (dimension value)
           (if value
               (lambda (game-state)
                 (declare (ignore game-state))
                 value)
               (lambda (game-state)
                 (option game-state (au:format-symbol ::keyword "WINDOW-~A"
                                                      dimension))))))
    (destructuring-bind (name &key point (type :render-buffer) width height) spec
      (make-instance 'framebuffer-attachment-definition
                     :name name
                     :type type
                     :point point
                     :width (generate-size-func :width width)
                     :height (generate-size-func :height height)))))

(defun find-framebuffer-attachment-definition (framebuffer attachment-name)
  (au:href (attachments framebuffer) attachment-name))

(defmacro define-framebuffer (name () &body body)
  (au:with-unique-names (definition attachment)
    (destructuring-bind (&key (mode :read/write) attachments) (car body)
      `(let* ((,definition (make-instance 'framebuffer-definition
                                          :name ',name
                                          :mode ',mode)))
         (dolist (spec ',attachments)
           (let ((,attachment (make-framebuffer-attachment-definition spec)))
             (setf (au:href (attachments ,definition) (name ,attachment))
                   ,attachment)))
         (setf (au:href *framebuffer-definitions* ',name) ,definition)))))

;;; Framebuffers

(defclass framebuffer ()
  ((%definition :reader definition
                :initarg :definition)
   (%name :reader name
          :initarg :name)
   (%id :reader id
        :initarg :id)
   (%mode :reader mode
          :initarg :mode)
   (%attachments :reader attachments
                 :initform (au:dict #'eq))))

(defun make-framebuffer (game-state definition)
  (with-slots (%name %mode) definition
    (let* ((id (gl:gen-framebuffer))
           (framebuffer (make-instance 'framebuffer
                                       :definition definition
                                       :id id
                                       :name %name
                                       :mode %mode)))
      (setf (au:href (framebuffers game-state) %name) framebuffer)
      framebuffer)))

(defun delete-framebuffer (framebuffer)
  (gl:delete-framebuffers (list (id framebuffer))))

(defun find-framebuffer (game-state name)
  (au:href (framebuffers game-state) name))

(defun framebuffer-mode->target (mode)
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

(defun framebuffer-attachment-point->gl (point)
  (destructuring-bind (type &optional (index 0)) (au:ensure-list point)
    (ecase type
      (:color (au:format-symbol :keyword "~a-ATTACHMENT~d" type index))
      (:depth :depth-attachment)
      (:stencil :stencil-attachment)
      (:depth/stencil :depth-stencil-attachment))))

(defun framebuffer-attachment-names->points (framebuffer attachment-names)
  (mapcar
   (lambda (x)
     (let ((attachment (find-framebuffer-attachment-definition
                        (definition framebuffer) x)))
       (framebuffer-attachment-point->gl (point attachment))))
   attachment-names))

(defun framebuffer-point->render-buffer-format (point)
  (destructuring-bind (type &optional index) (au:ensure-list point)
    (declare (ignore index))
    (ecase type
      (:color :rgb)
      (:depth :depth-component)
      (:stencil :stencil-index)
      (:depth/stencil :depth24-stencil8))))

(defun ensure-framebuffer-complete (framebuffer target buffer attachment)
  (let ((result (gl:check-framebuffer-status target)))
    (unless (member result '(:framebuffer-complete :framebuffer-complete-oes))
      (error "Error attaching ~a as attachment ~a of framebuffer ~a: ~a"
             buffer attachment (name framebuffer) result))))

(defun framebuffer-attach (game-state framebuffer attachment-name)
  (let* ((definition (definition framebuffer))
         (attachment (find-framebuffer-attachment-definition
                      definition attachment-name)))
    (ecase (attachment-type attachment)
      (:render-buffer (framebuffer-attach/render-buffer
                       game-state framebuffer attachment))
      (:texture (framebuffer-attach/texture
                 game-state framebuffer attachment)))))

(defun find-framebuffer-texture-id (game-state framebuffer-name attachment-name)
  (let* ((framebuffer (find-framebuffer game-state framebuffer-name))
         (definition (definition framebuffer))
         (attachment (find-framebuffer-attachment-definition
                      definition attachment-name))
         (point (framebuffer-attachment-point->gl (point attachment))))
    (au:href (attachments framebuffer) point)))

(defun initialize-framebuffers (game-state)
  (au:do-hash-values (definition *framebuffer-definitions*)
    (with-slots (%attachments) definition
      (let ((framebuffer (make-framebuffer game-state definition)))
        (au:do-hash-values (attachment %attachments)
          (framebuffer-attach game-state framebuffer (name attachment)))))))

(defmacro with-framebuffer ((framebuffer) &body body)
  (au:with-unique-names (target)
    `(let ((,target (framebuffer-mode->target (mode ,framebuffer))))
       (gl:bind-framebuffer ,target (id ,framebuffer))
       ,@body
       (gl:bind-framebuffer ,target 0))))

;;; Render buffer attachments

(defun framebuffer-attach/render-buffer (game-state framebuffer attachment)
  (with-slots (%point %width %height) attachment
    (let* ((framebuffer-target (framebuffer-mode->target (mode framebuffer)))
           (target :renderbuffer)
           (internal-format (framebuffer-point->render-buffer-format %point))
           (point (framebuffer-attachment-point->gl %point))
           (buffer-id (gl:gen-renderbuffer))
           (width (funcall %width game-state))
           (height (funcall %height game-state)))
      (gl:bind-renderbuffer target buffer-id)
      (gl:renderbuffer-storage target internal-format width height)
      (gl:bind-renderbuffer target 0)
      (with-framebuffer (framebuffer)
        (gl:framebuffer-renderbuffer framebuffer-target point target buffer-id)
        (ensure-framebuffer-complete
         framebuffer framebuffer-target buffer-id point))
      (setf (au:href (attachments framebuffer) point) buffer-id)
      buffer-id)))

;;; Texture attachments

(defun framebuffer-attach/texture (game-state framebuffer attachment)
  (with-slots (%point %width %height) attachment
    (let* ((target (framebuffer-mode->target (mode framebuffer)))
           (width (funcall %width game-state))
           (height (funcall %height game-state))
           (buffer-id (load-texture game-state
                                    (au:format-symbol :bloom "FRAMEBUFFER-~a"
                                                      (car %point))
                                    :width width
                                    :height height))
           (point (framebuffer-attachment-point->gl %point)))
      (with-framebuffer (framebuffer)
        (%gl:framebuffer-texture target point buffer-id 0)
        (ensure-framebuffer-complete framebuffer target buffer-id point))
      (setf (au:href (attachments framebuffer) point) buffer-id)
      buffer-id)))
