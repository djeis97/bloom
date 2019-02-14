(in-package :bloom)

(defgeneric perform-task (core type data))

(defun schedule-task (core type data)
  (queues:qpush (tasks core) (list type data)))

(defun process-tasks (core)
  (loop :with queue = (tasks core)
        :for ((type data) . found-p) = (multiple-value-list (queues:qpop queue))
        :while found-p
        :do (perform-task core type data)))
