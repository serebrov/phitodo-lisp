;;;; views.lisp - View implementations

(in-package #:phitodo-tui.ui)

;;; View: Inbox
(defun view-inbox (win x y width height)
  "Render inbox view."
  (let ((tasks (phitodo-tui.services:filter-inbox *tasks*)))
    (draw-task-list win x y width height tasks "Inbox")))

;;; View: Today
(defun view-today (win x y width height)
  "Render today view."
  (let ((tasks (phitodo-tui.services:filter-today *tasks*)))
    (draw-task-list win x y width height tasks "Today")))

;;; View: Upcoming
(defun view-upcoming (win x y width height)
  "Render upcoming view."
  (let ((tasks (phitodo-tui.services:filter-upcoming *tasks*)))
    (draw-task-list win x y width height
                    (phitodo-tui.services:sort-by-due-date tasks) "Upcoming")))

;;; View: Anytime
(defun view-anytime (win x y width height)
  "Render anytime view."
  (let ((tasks (phitodo-tui.services:filter-anytime *tasks*)))
    (draw-task-list win x y width height tasks "Anytime")))

;;; View: Completed
(defun view-completed (win x y width height)
  "Render completed view."
  (let ((tasks (phitodo-tui.services:filter-completed *tasks*)))
    (draw-task-list win x y width height tasks "Completed")))

;;; View: Review
(defun view-review (win x y width height)
  "Render review view for overdue tasks."
  (let ((tasks (phitodo-tui.services:filter-review *tasks*)))
    (draw-task-list win x y width height tasks "Review (Overdue)")))

;;; View: Project
(defun view-project (win x y width height project-id)
  "Render project view."
  (let* ((project (find project-id *projects*
                        :key #'phitodo-tui.models:project-id
                        :test #'equal))
         (tasks (phitodo-tui.services:filter-by-project *tasks* project-id))
         (title (if project
                    (phitodo-tui.models:project-name project)
                    "Project")))
    (draw-task-list win x y width height tasks title)))

;;; View: Tag
(defun view-tag (win x y width height tag-id)
  "Render tag view."
  (let* ((tag (find tag-id *tags*
                    :key #'phitodo-tui.models:tag-id
                    :test #'equal))
         (tasks (phitodo-tui.services:filter-by-tag *tasks* tag-id))
         (title (if tag
                    (phitodo-tui.models:tag-name tag)
                    "Tag")))
    (draw-task-list win x y width height tasks title)))

;;; View: GitHub
(defun draw-github-section (win x row width max-row title items color)
  "Draw a section of GitHub items. Returns the new row position."
  (when (< row max-row)
    (move win row x)
    (setf (attributes win) '(:bold))
    (setf (color-pair win) (list color :white))
    (format win "~A (~A)" title (length items))
    (setf (color-pair win) '(:black :white))
    (setf (attributes win) '())
    (incf row))
  (loop for task in items
        while (< row max-row) do
        (move win row x)
        (format win "  ~A" (truncate-string
                            (phitodo-tui.models:task-title task)
                            (- width 4)))
        (incf row))
  (incf row) ; blank line
  row)

(defun view-github (win x y width height)
  "Render GitHub integration view."
  (let ((row y)
        (max-row (+ y height -2))
        (has-data (or *github-issues* *github-prs* *github-reviews*)))
    ;; Title
    (move win row x)
    (setf (attributes win) '(:bold))
    (format win "GitHub")
    (setf (attributes win) '())
    (incf row 2)

    (if (phitodo-tui.config:config-github-token *config*)
        (if has-data
            (progn
              ;; Review Requests (most urgent)
              (when *github-reviews*
                (setf row (draw-github-section win x row width max-row
                                               "Review Requests" *github-reviews* :red)))
              ;; My PRs
              (when *github-prs*
                (setf row (draw-github-section win x row width max-row
                                               "My Pull Requests" *github-prs* :blue)))
              ;; Issues
              (when *github-issues*
                (setf row (draw-github-section win x row width max-row
                                               "Assigned Issues" *github-issues* :green))))
            ;; Token configured but no data yet
            (progn
              (move win row x)
              (setf (color-pair win) '(:green :white))
              (format win "GitHub token configured")
              (setf (color-pair win) '(:black :white))
              (incf row 2)

              (move win row x)
              (format win "Press 'r' to fetch data from GitHub.")))
        (progn
          ;; No token
          (move win row x)
          (format win "GitHub integration")
          (incf row)

          (move win row x)
          (setf (color-pair win) '(:black :white))
          (format win "Configure token in Settings (9)")))))

;;; View: Toggl
(defun view-toggl (win x y width height)
  "Render Toggl time tracking view."
  (declare (ignore height))
  (let ((row y))
    ;; Title
    (move win row x)
    (setf (attributes win) '(:bold))
    (format win "Toggl")
    (setf (attributes win) '())
    (incf row 2)

    (if (phitodo-tui.config:config-toggl-token *config*)
        (progn
          ;; Token configured
          (move win row x)
          (setf (color-pair win) '(:green :white))
          (format win "Toggl token configured")
          (setf (color-pair win) '(:black :white))
          (incf row 2)

          (move win row x)
          (format win "Toggl integration is ready.")
          (incf row)
          (move win row x)
          (format win "Press 'r' to refresh time entries."))
        (progn
          ;; No token
          (move win row x)
          (format win "Toggl time tracking")
          (incf row)

          (move win row x)
          (setf (color-pair win) '(:black :white))
          (format win "Configure token in Settings (9)")))))

;;; View: Settings
(defvar *settings-fields* '(:db-path :github-token :toggl-token))

(defun view-settings (win x y width height config)
  "Render settings view."
  (declare (ignore height))
  (let ((row y))
    ;; Title
    (move win row x)
    (setf (attributes win) '(:bold))
    (format win "Settings")
    (setf (attributes win) '())
    (incf row 2)

    ;; Database path
    (move win row x)
    (when (and (eq *focus* :list) (= *settings-selected* 0))
      (setf (attributes win) '(:bold))
      (format win "> ")
      (setf (attributes win) '()))
    (format win "Database: ")
    (setf (color-pair win) '(:cyan :black))
    (format win "~A" (or (phitodo-tui.config:config-db-path config) "(default)"))
    (setf (color-pair win) '(:black :white))
    (incf row 2)

    ;; GitHub token
    (move win row x)
    (when (and (eq *focus* :list) (= *settings-selected* 1))
      (setf (attributes win) '(:bold))
      (format win "> ")
      (setf (attributes win) '()))
    (format win "GitHub Token: ")
    (if (phitodo-tui.config:config-github-token config)
        (progn
          (setf (color-pair win) '(:green :black))
          (format win "***configured***"))
        (progn
          (setf (color-pair win) '(:black :white))
          (format win "(not set)")))
    (setf (color-pair win) '(:black :white))
    (incf row 2)

    ;; Toggl token
    (move win row x)
    (when (and (eq *focus* :list) (= *settings-selected* 2))
      (setf (attributes win) '(:bold))
      (format win "> ")
      (setf (attributes win) '()))
    (format win "Toggl Token: ")
    (if (phitodo-tui.config:config-toggl-token config)
        (progn
          (setf (color-pair win) '(:green :black))
          (format win "***configured***"))
        (progn
          (setf (color-pair win) '(:black :white))
          (format win "(not set)")))
    (setf (color-pair win) '(:black :white))
    (incf row 3)

    ;; Help text
    (move win row x)
    (setf (color-pair win) '(:black :white))
    (format win "Press Enter to edit selected field")))

;;; Main view dispatcher
(defun get-current-tasks ()
  "Get tasks for the current view."
  (case *current-view*
    (:inbox (phitodo-tui.services:filter-inbox *tasks*))
    (:today (phitodo-tui.services:filter-today *tasks*))
    (:upcoming (phitodo-tui.services:sort-by-due-date
                (phitodo-tui.services:filter-upcoming *tasks*)))
    (:anytime (phitodo-tui.services:filter-anytime *tasks*))
    (:completed (phitodo-tui.services:filter-completed *tasks*))
    (:review (phitodo-tui.services:filter-review *tasks*))
    (otherwise *tasks*)))

(defun get-selected-task ()
  "Get the currently selected task."
  (let ((tasks (get-current-tasks)))
    (when (and tasks (>= *selected-index* 0) (< *selected-index* (length tasks)))
      (nth *selected-index* tasks))))

(defun render-current-view (win x y width height config)
  "Render the current view."
  (case *current-view*
    (:inbox (view-inbox win x y width height))
    (:today (view-today win x y width height))
    (:upcoming (view-upcoming win x y width height))
    (:anytime (view-anytime win x y width height))
    (:completed (view-completed win x y width height))
    (:review (view-review win x y width height))
    (:github (view-github win x y width height))
    (:toggl (view-toggl win x y width height))
    (:settings (view-settings win x y width height config))
    (otherwise (view-inbox win x y width height))))
