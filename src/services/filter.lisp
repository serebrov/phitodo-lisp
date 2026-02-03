;;;; filter.lisp - Task filtering and sorting

(in-package #:phitodo-tui.services)

(defun filter-inbox (tasks)
  "Get tasks in inbox status."
  (remove-if-not (lambda (task)
                   (eq (task-status task) :inbox))
                 tasks))

(defun filter-today (tasks)
  "Get tasks due today or overdue (not completed)."
  (let ((today (local-time:today)))
    (remove-if-not (lambda (task)
                     (and (task-due-date task)
                          (not (task-completed-p task))
                          (local-time:timestamp<= (task-due-date task) today)))
                   tasks)))

(defun filter-upcoming (tasks)
  "Get tasks with future due dates (not completed)."
  (let ((today (local-time:today)))
    (remove-if-not (lambda (task)
                     (and (task-due-date task)
                          (not (task-completed-p task))
                          (local-time:timestamp> (task-due-date task) today)))
                   tasks)))

(defun filter-anytime (tasks)
  "Get tasks without due dates (not completed)."
  (remove-if-not (lambda (task)
                   (and (null (task-due-date task))
                        (not (task-completed-p task))))
                 tasks))

(defun filter-completed (tasks)
  "Get completed tasks."
  (remove-if-not #'task-completed-p tasks))

(defun filter-review (tasks)
  "Get overdue tasks for review."
  (remove-if-not #'task-overdue-p tasks))

(defun filter-by-project (tasks project-id)
  "Get tasks for a specific project."
  (remove-if-not (lambda (task)
                   (equal (task-project-id task) project-id))
                 tasks))

(defun filter-by-tag (tasks tag-id)
  "Get tasks with a specific tag."
  (remove-if-not (lambda (task)
                   (member tag-id (task-tags task) :test #'equal))
                 tasks))

(defun search-tasks (tasks query)
  "Search tasks by title or notes."
  (let ((query-lower (string-downcase query)))
    (remove-if-not (lambda (task)
                     (or (search query-lower (string-downcase (task-title task)))
                         (and (task-notes task)
                              (search query-lower (string-downcase (task-notes task))))))
                   tasks)))

(defun sort-by-due-date (tasks)
  "Sort tasks by due date (nulls last)."
  (sort (copy-list tasks)
        (lambda (a b)
          (cond
            ((and (null (task-due-date a)) (null (task-due-date b))) nil)
            ((null (task-due-date a)) nil)
            ((null (task-due-date b)) t)
            (t (local-time:timestamp< (task-due-date a) (task-due-date b)))))))

(defun sort-by-priority (tasks)
  "Sort tasks by priority (high first)."
  (let ((priority-order '(:high :medium :low :none)))
    (sort (copy-list tasks)
          (lambda (a b)
            (< (position (task-priority a) priority-order)
               (position (task-priority b) priority-order))))))
