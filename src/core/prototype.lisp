(in-package :bloom)

(defvar *prototypes* (au:dict #'eq))

(defclass prototype ()
  ((%func :accessor func
          :initarg :func
          :initform (constantly nil))))

(defun find-prototype (name)
  (au:href *prototypes* name))

(defmacro define-prototype (name () &body body)
  (au:with-unique-names (entity entity-id component func game-state)
    (flet ((%generate-attachment (component-spec)
             (destructuring-bind (type . args) component-spec
               (let ((id (au:format-symbol *package* "~a" type)))
                 `(let ((,component (make-component ,game-state ',type ,@args :id ',id)))
                    (attach-component ,entity ,component))))))
      (let ((component-attachments (mapcar #'%generate-attachment body)))
        (unless (find 'transform body :key #'car)
          (push (%generate-attachment '(transform)) component-attachments))
        `(let ((,func (lambda (,game-state ,entity-id)
                        (let ((,entity (%make-entity ,game-state ',name ,entity-id)))
                          ,@component-attachments
                          ,entity))))
           (setf (au:href *prototypes* ',name) (make-instance 'prototype :func ,func)))))))
