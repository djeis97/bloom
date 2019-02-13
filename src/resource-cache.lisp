(in-package :bloom)

(defmacro cache-lookup (game-state type key &body body)
  (au:with-unique-names (data value found-p)
    `(symbol-macrolet ((,data (au:href (resource-cache ,game-state) ,type)))
       (multiple-value-bind (,value ,found-p) ,data
         (declare (ignore ,value))
         (unless ,found-p
           (setf ,data (au:dict #'equalp))))
       (au:ensure-gethash ,key ,data (progn ,@body)))))
