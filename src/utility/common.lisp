(in-package :bloom)

(defvar *release-p* nil)

#+sbcl
(defun deploy-binary (file-name)
  (v:stop v:*global-controller*)
  (setf *release-p* t)
  (sb-ext:save-lisp-and-die file-name
                            :toplevel 'start-engine
                            :executable t
                            :compression 9))

(defmacro with-profile (&body body)
  (let ((packages (remove-if-not
                   (lambda (x) (au:string-starts-with-p x "BLOOM."))
                   (mapcar #'package-name (list-all-packages)))))
    `(progn
       (sb-profile:unprofile)
       (sb-profile:profile ,@packages "AU" "GAME-MATH" "SHADOW" "BLOOM")
       ,@body
       (sb-profile:report)
       (sb-profile:unprofile)
       (sb-profile:reset))))
