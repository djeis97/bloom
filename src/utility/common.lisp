(in-package :bloom)

(defvar *release-p* nil)

#+sbcl
(defun deploy-binary (file-name)
  (v:stop v:*global-controller*)
  (setf *release-p* t)
  (sb-ext:save-lisp-and-die file-name :toplevel 'start-engine :executable t :compression 9))

(defmacro with-profile (&body body)
  (let ((packages (remove-if-not
                   (lambda (x) (or (au:string-starts-with-p x "BLOOM.")
                                   (au:string-starts-with-p x "BOX.MATH.")))
                   (mapcar #'package-name (list-all-packages)))))
    `(progn
       (sb-profile:unprofile)
       (sb-profile:profile ,@packages "AU" "SHADOW" "BLOOM")
       ,@body
       (sb-profile:report)
       (sb-profile:unprofile)
       (sb-profile:reset))))
