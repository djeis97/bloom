(in-package :bloom)

(au:define-constant +mouse-button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defclass mouse-motion-state ()
  ((%x :accessor x
       :initform 0)
   (%y :accessor y
       :initform 0)
   (%dx :accessor dx
        :initform 0)
   (%dy :accessor dy
        :initform 0)))

;;; Events

(defun on-mouse-button-up (input-data button)
  (input-transition-out input-data (list :mouse button))
  (input-transition-out input-data '(:mouse :any))
  (input-transition-out input-data '(:button :any)))

(defun on-mouse-button-down (input-data button)
  (input-transition-in input-data (list :mouse button))
  (input-transition-in input-data '(:mouse :any))
  (input-transition-in input-data '(:button :any)))

(defun on-mouse-scroll (input-data x y)
  (let ((states (states input-data)))
    (unless (zerop x)
      (setf (au:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (au:href states '(:mouse :scroll-vertical)) y))))

(defun on-mouse-move (input-data new-x new-y new-dx new-dy)
  (symbol-macrolet ((state (au:href (states input-data) '(:mouse :motion))))
    (if state
        (setf (x state) new-x
              (y state) new-y
              (dx state) new-dx
              (dy state) new-dy)
        (setf state (make-instance 'mouse-motion-state)))))

;;; User protocol

(defun get-mouse-position (input-data)
  (let ((state (au:href (states input-data) '(:mouse :motion))))
    (values (x state)
            (y state)
            (dx state)
            (dy state))))

(defun get-mouse-scroll (input-data axis)
  (let ((states (states input-data)))
    (case axis
      (:horizontal (au:href states '(:mouse :scroll-horizontal)))
      (:vertical (au:href states '(:mouse :scroll-vertical))))))
