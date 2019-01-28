(in-package :bloom)

(defclass state ()
  ((%enter :accessor state-enter
           :initform t)
   (%enabled :accessor state-enabled
             :initform t)
   (%exit :accessor state-exit
          :initform nil)))

(defclass input-data ()
  ((%entering :accessor entering
              :initform nil)
   (%exiting :accessor exiting
             :initform nil)
   (%states :reader states
            :initform (au:dict #'equal
                               '(:mouse :scroll-horizontal) 0
                               '(:mouse :scroll-vertical) 0))))

(defun input-transition-in (input-data input)
  (symbol-macrolet ((state (au:href (states input-data) input)))
    (if state
        (setf (state-enter state) t
              (state-enabled state) t
              (state-exit state) nil)
        (setf state (make-instance 'state)))
    (push input (entering input-data))))

(defun input-transition-out (input-data input)
  (au:when-let ((state (au:href (states input-data) input)))
    (setf (state-enter state) nil
          (state-enabled state) nil
          (state-exit state) t)
    (push input (exiting input-data))))

(defun enable-entering (input-data)
  (symbol-macrolet ((entering (entering input-data)))
    (dolist (input entering)
      (let ((state (au:href (states input-data) input)))
        (setf (state-enter state) nil
              (state-enabled state) t
              (state-exit state) nil)))
    (setf entering nil)))

(defun disable-exiting (input-data)
  (symbol-macrolet ((exiting (exiting input-data)))
    (dolist (input exiting)
      (let ((state (au:href (states input-data) input)))
        (setf (state-enter state) nil
              (state-enabled state) nil
              (state-exit state) nil)))
    (setf exiting nil)))

(defun input-enter-p (game-state input)
  (au:when-let ((state (au:href (states (input-data game-state)) input)))
    (state-enter state)))

(defun input-enabled-p (game-state input)
  (au:when-let ((state (au:href (states (input-data game-state)) input)))
    (state-enabled state)))

(defun input-exit-p (game-state input)
  (au:when-let ((state (au:href (states (input-data game-state)) input)))
    (state-exit state)))
