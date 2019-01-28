(in-package :bloom)

(defgeneric perform-task (game-state type data))

(defun schedule-task (game-state type data)
  (queues:qpush (tasks game-state) (list type data)))

(defun process-tasks (game-state)
  (loop :with queue = (tasks game-state)
        :for ((type data) . found-p) = (multiple-value-list (queues:qpop queue))
        :while found-p
        :do (perform-task game-state type data)))
