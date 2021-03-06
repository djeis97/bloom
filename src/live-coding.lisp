(in-package :bloom)

(defmacro with-continue-restart (report &body body)
  `(let* ((debugger-entry-time)
          (previous-hook *debugger-hook*)
          (#+sbcl sb-ext:*invoke-debugger-hook*
           #-sbcl *debugger-hook
           (lambda (condition hook)
             (declare (ignore hook))
             (setf debugger-entry-time (get-time))
             (when previous-hook
               (funcall previous-hook condition previous-hook)))))
     (restart-case (progn ,@body)
       (continue ()
         :report ,report
         (with-slots (%pause-time) (frame-manager *core*)
           (when debugger-entry-time
             (setf %pause-time (- (get-time) debugger-entry-time))))))))

(defun compile-live-coding-functions ()
  (au:if-let ((repl-package (find-if #'find-package '(:slynk :swank))))
    (progn
      (compile 'find-lisp-repl
               `(lambda ()
                  (when ,repl-package
                    (load-time-value
                     (or ,(au:ensure-symbol "*EMACS-CONNECTION*" repl-package)
                         (,(au:ensure-symbol "DEFAULT-CONNECTION"
                                             repl-package)))))))
      (compile 'setup-lisp-repl
               `(lambda ()
                  (when ,(eq repl-package :slynk)
                    (,(au:ensure-symbol "SEND-PROMPT" "SLYNK-MREPL")
                     (find (bt:current-thread)
                           (,(au:ensure-symbol "CHANNELS" repl-package))
                           :key #',(au:ensure-symbol "CHANNEL-THREAD"
                                                     repl-package))))))
      (compile 'update-lisp-repl
               `(lambda ()
                  (when ,repl-package
                    (au:when-let ((repl (find-lisp-repl)))
                      (with-continue-restart "REPL"
                        (,(au:ensure-symbol "HANDLE-REQUESTS" repl-package)
                         repl t)))))))
    (progn
      (compile 'setup-lisp-repl '(constantly nil))
      (compile 'update-lisp-repl '(constantly nil)))))

(defun setup-live-coding ()
  (compile-live-coding-functions)
  (funcall 'setup-lisp-repl))

(defun live-coding-update ()
  (funcall 'update-lisp-repl))
