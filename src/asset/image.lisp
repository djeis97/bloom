(in-package :bloom)

(defclass image ()
  ((%path :reader path
          :initarg :path)
   (%type :reader image-type
          :initarg :type)
   (%surface :reader surface
             :initarg :surface)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%channels :reader channels
              :initarg :channels)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type
                :initform :unsigned-byte)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data)))

(defun make-image (&rest init-args)
  (apply #'make-instance 'image init-args))

(defun get-image-extension-keyword (path)
  (au:make-keyword (string-upcase (pathname-type path))))

(defun get-loader-type (path)
  (let ((extension (get-image-extension-keyword path)))
    (ecase extension
      ((:tga :bmp :pbm :pgm :ppm :xpm :xcf :pcx :gif :jpg :jpeg :tif :tiff :lbm :iff :png)
       :sdl2-image))))

(defmethod get-pixel-size ((image image))
  (ecase (pixel-format image)
    (:red 1)
    ((:rgb :bgr) 3)
    ((:rgba :bgra) 4)))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    ((:rgb :bgr) :rgb8)
    ((:rgba :bgra) :rgba8)))

(defgeneric %read-image (loader path))

(defun read-image (path)
  (%read-image (get-loader-type path) path))

(defgeneric %free-storage (loader image))

(defun free-storage (image)
  (%free-storage (get-loader-type (path image)) image))

(defmacro using-image (binding path &body body)
  `(let ((,binding (read-image ,path)))
     (unwind-protect ,@body
       (free-storage ,binding))
     ,binding))

;;; sdl2-image backend (default)

(defun get-surface-channel-count (surface)
  (let ((format (sdl2:surface-format-format surface)))
    (ecase format
      ((:index8) 1)
      ((:rgb24 :bgr24 :rgb888 :bgr888) 3)
      ((:argb8888 :rgba8888 :abgr8888 :bgra8888 :rgba32 :argb32 :bgra32 :abgr32) 4))))

(defun get-surface-pixel-format (surface)
  (let ((format (sdl2:surface-format-format surface)))
    (ecase format
      (:index8 :red)
      ((:rgb24 :rgb888)
       :rgb)
      ((:bgr24 :bgr888)
       :bgr)
      ((:rgba8888 :abgr8888 :rgba32 :abgr32)
       :rgba)
      ((:argb8888 :bgra8888 :argb32 :bgra32)
       :bgra))))

(defmethod %read-image ((loader (eql :sdl2-image)) path)
  (let* ((surface (sdl2-image:load-image path))
         (pixel-format (get-surface-pixel-format surface))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface))
         (channels (get-surface-channel-count surface)))
    (make-image :path path
                :type (get-image-extension-keyword path)
                :surface surface
                :width width
                :height height
                :channels channels
                :pixel-format pixel-format
                :internal-format (get-internal-format pixel-format)
                :data (sdl2:surface-pixels surface))))

(defmethod %free-storage ((loader (eql :sdl2-image)) image)
  (with-slots (%surface %data) image
    (sdl2:free-surface %surface)
    (setf %surface nil
          %data nil)))
