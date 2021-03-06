(in-package :bloom)

(defmacro event-case ((event) &body handlers)
  `(case (sdl2:get-event-type ,event)
     ,@(au:collecting
         (dolist (handler handlers)
           (destructuring-bind (type options . body) handler
             (let ((body (list*
                          `(declare (ignorable ,@(au:plist-values options)))
                          body)))
               (dolist (type (au:ensure-list type))
                 (au:when-let ((x (sdl2::expand-handler
                                   event type options body)))
                   (collect x)))))))))

(defun dispatch-event (input-data event)
  (event-case (event)
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (on-window-show))
       (:hide (on-window-hide))
       (:move (on-window-move :x data1 :y data2))
       (:resize (on-window-resize :width data1 :height data2))
       (:minimize (on-window-minimize))
       (:maximize (on-window-maximize))
       (:restore (on-window-restore))
       (:mouse-focus-enter (on-window-mouse-focus-enter))
       (:mouse-focus-leave (on-window-mouse-focus-exit))
       (:keyboard-focus-enter (on-window-keyboard-focus-enter))
       (:keyboard-focus-leave (on-window-keyboard-focus-exit))
       (:close (on-window-close))))
    (:mousebuttonup
     (:button button)
     (on-mouse-button-up input-data (aref +mouse-button-names+ button)))
    (:mousebuttondown
     (:button button)
     (on-mouse-button-down input-data (aref +mouse-button-names+ button)))
    (:mousewheel
     (:x x :y y)
     (on-mouse-scroll input-data x y))
    (:mousemotion
     (:x x :y y :xrel dx :yrel dy)
     (on-mouse-move input-data x y dx dy))
    (:keyup
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (on-key-up input-data (aref +key-names+ (sdl2:scancode-value keysym)))))
    (:keydown
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (on-key-down input-data (aref +key-names+ (sdl2:scancode-value keysym)))))
    (:controllerdeviceadded
     (:which index)
     (on-gamepad-attach input-data index))
    (:controllerdeviceremoved
     (:which gamepad-id)
     (on-gamepad-detach input-data gamepad-id))
    (:controlleraxismotion
     (:which gamepad-id :axis axis :value value)
     (on-gamepad-analog-move
      input-data gamepad-id (aref +gamepad-axis-names+ axis) value))
    (:controllerbuttonup
     (:which gamepad-id :button button)
     (on-gamepad-button-up
      input-data gamepad-id (aref +gamepad-button-names+ button)))
    (:controllerbuttondown
     (:which gamepad-id :button button)
     (on-gamepad-button-down
      input-data gamepad-id (aref +gamepad-button-names+ button)))))

(defun perform-input-state-tasks (input-data)
  (let ((states (au:href (states input-data))))
    (setf (au:href states '(:mouse :scroll-horizontal)) 0
          (au:href states '(:mouse :scroll-vertical)) 0)
    (enable-entering input-data)
    (disable-exiting input-data)))

(defun handle-events (core)
  (with-slots (%input-data) core
    (perform-input-state-tasks %input-data)
    (loop :with event = (sdl2:new-event)
          :until (zerop (sdl2:next-event event :poll))
          :do (dispatch-event %input-data event)
          :finally (sdl2:free-event event))))
