(in-package :bloom)

(defclass image ()
  ((%type :reader image-type
          :initarg :type)
   (%file :reader file
          :initarg :file)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%data-type :reader data-type
               :initarg :data-type)
   (%data :accessor data
          :initarg :data)))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    ((:rgb :bgr) :rgb8)
    ((:rgba :bgra) :rgba8)))

(defmethod get-pixel-format ((image-type (eql :tga)) data)
  (ecase data
    (1 :red)
    (3 :bgr)
    (4 :bgra)))

(defmethod %read-image ((image-type (eql :tga)) path)
  (let* ((image (tga:read-tga path))
         (pixel-format (get-pixel-format image-type (tga:image-channels image))))
    (make-instance 'image
                   :type image-type
                   :file path
                   :width (tga:image-width image)
                   :height (tga:image-height image)
                   :internal-format (get-internal-format pixel-format)
                   :pixel-format pixel-format
                   :data-type :unsigned-byte
                   :data (tga:image-data image))))

(defmethod get-pixel-format ((image-type (eql :png)) data)
  (ecase data
    (:greyscale :red)
    (:greyscale-alpha :rg)
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defmethod %read-image ((image-type (eql :png)) path)
  (let* ((image (pngload:load-file path :flatten t :flip-y nil))
         (pixel-format (get-pixel-format image-type (pngload:color-type image))))
    (make-instance 'image
                   :type image-type
                   :file path
                   :width (pngload:width image)
                   :height (pngload:height image)
                   :internal-format (get-internal-format pixel-format)
                   :pixel-format pixel-format
                   :data-type :unsigned-byte
                   :data (pngload:data image))))

(defun read-image (path)
  (let* ((path (resolve-path :image path))
         (file-type (au:make-keyword (string-upcase (pathname-type path)))))
    (%read-image file-type path)))

(defmacro using-image (binding path &body body)
  `(let ((,binding (read-image ,path)))
     (unwind-protect ,@body
       (setf (data ,binding) nil))
     ,binding))
