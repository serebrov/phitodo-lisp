;;;; components.lisp - UI components

(in-package #:phitodo-tui.ui)

;;; Forward declarations for app state
(defvar *current-view* :inbox)
(defvar *selected-index* 0)
(defvar *sidebar-index* 0)
(defvar *settings-selected* 0)
(defvar *focus* :sidebar)
(defvar *tasks* nil)
(defvar *projects* nil)
(defvar *tags* nil)
(defvar *show-help* nil)
(defvar *status-message* nil)
(defvar *screen* nil)
(defvar *config* nil)
(defvar *github-issues* nil)
(defvar *github-prs* nil)
(defvar *github-reviews* nil)

;;; Task Form State
(defvar *task-form-mode* nil "Whether task form is active")
(defvar *task-form-task* nil "The task being edited")
(defvar *task-form-is-new* nil "Whether this is a new task")
(defvar *task-form-field* 0 "Current field index (0-7)")
(defvar *task-form-title* "" "Title input")
(defvar *task-form-notes* "" "Notes input")
(defvar *task-form-due-date* "" "Due date input (YYYY-MM-DD)")

(defparameter *task-form-fields*
  '(:title :notes :due-date :project :priority :status :kind :size)
  "All task form fields")

;;; Utility functions
(defun truncate-string (str max-len)
  "Truncate string to max-len, adding ellipsis if needed."
  (if (> (length str) max-len)
      (concatenate 'string (subseq str 0 (max 0 (- max-len 3))) "...")
      str))

(defun format-date (timestamp)
  "Format timestamp for display."
  (if timestamp
      (local-time:format-timestring nil timestamp
                                    :format '(:short-month " " :day)
                                    :timezone local-time:+utc-zone+)
      ""))

(defun safe-subseq (seq start &optional end)
  "Safe subsequence that handles bounds."
  (let ((len (length seq)))
    (subseq seq
            (min start len)
            (if end (min end len) len))))

;;; Component: Sidebar
(defun draw-sidebar (win x y width height)
  "Draw the sidebar navigation."
  (declare (ignore x))
  (let ((row y))
    ;; Title
    (move win row 0)
    (setf (attributes win) '(:bold))
    (format win "PHITODO")
    (setf (attributes win) '())
    (incf row 2)

    ;; Navigation items
    (loop for item in *sidebar-items*
          for idx from 0 do
      (when (< row (+ y height -1))
        (let* ((is-selected (and (eq *focus* :sidebar)
                                 (= idx *sidebar-index*)))
               (icon (sidebar-item-icon item))
               (label (sidebar-item-label item))
               (shortcut (sidebar-item-shortcut item))
               (line (format nil "~A ~A" icon label)))
          (move win row 0)
          (when is-selected
            (setf (color-pair win) '(:white :blue)))
          (format win "~A" (truncate-string line (- width 4)))
          (setf (color-pair win) '(:black :white))
          ;; Shortcut on the right
          (when shortcut
            (move win row (- width 3))
            (setf (color-pair win) '(:black :white))
            (format win "~A" shortcut)))
        (incf row)))

    ;; Draw vertical separator
    (loop for r from y below (+ y height) do
          (move win r (- width 1))
          (princ #\| win))))

;;; Component: Task List
(defun draw-task-list (win x y width height tasks title)
  "Draw a list of tasks."
  (let ((row y)
        (visible-tasks (safe-subseq tasks 0 (- height 2))))
    ;; Title
    (move win row x)
    (setf (attributes win) '(:bold))
    (format win "~A (~A)" title (length tasks))
    (setf (attributes win) '())
    (incf row 2)

    ;; Tasks
    (loop for task in visible-tasks
          for idx from 0 do
          (when (< row (+ y height -1))
            (let* ((is-selected (and (eq *focus* :list)
                                     (= idx *selected-index*)))
                   (status-sym (status-symbol (phitodo-tui.models:task-status task)))
                   (priority-sym (priority-symbol (phitodo-tui.models:task-priority task)))
                   (title-str (phitodo-tui.models:task-title task))
                   (due-str (format-date (phitodo-tui.models:task-due-date task)))
                   (max-title-len (max 5 (- width (length status-sym) (length priority-sym)
                                            (length due-str) 6)))
                   (line (format nil "~A ~A~A ~A"
                                 status-sym
                                 (if (> (length priority-sym) 0)
                                     (concatenate 'string priority-sym " ")
                                     "")
                                 (truncate-string title-str max-title-len)
                                 due-str)))
              (move win row x)
              (when is-selected
                (setf (color-pair win) '(:white :blue)))
              (format win "~A" (truncate-string line (- width 2)))
              (setf (color-pair win) '(:black :white)))
            (incf row)))

    ;; Empty state
    (when (null tasks)
      (move win (+ y 2) x)
      (setf (color-pair win) '(:black :white))
      (format win "No tasks"))))

;;; Component: Task Detail
(defun word-wrap (text width)
  "Wrap text to specified width, returning list of lines."
  (if (or (null text) (string= text ""))
      nil
      (let ((lines nil)
            (current-line "")
            (words (uiop:split-string text :separator '(#\Space #\Newline #\Return))))
        (dolist (word words)
          (cond
            ;; Empty word (from consecutive spaces/newlines)
            ((string= word "") nil)
            ;; First word on line
            ((string= current-line "")
             (setf current-line word))
            ;; Word fits on current line
            ((<= (+ (length current-line) 1 (length word)) width)
             (setf current-line (concatenate 'string current-line " " word)))
            ;; Word doesn't fit, start new line
            (t
             (push current-line lines)
             (setf current-line word))))
        ;; Don't forget last line
        (when (> (length current-line) 0)
          (push current-line lines))
        (nreverse lines))))

(defun format-timestamp (timestamp)
  "Format timestamp as YYYY-MM-DD HH:MM (UTC)."
  (if timestamp
      (local-time:format-timestring nil timestamp
                                    :format '(:year "-" (:month 2) "-" (:day 2)
                                              " " (:hour 2) ":" (:min 2))
                                    :timezone local-time:+utc-zone+)
      ""))

(defun draw-task-detail-view (win x y width height task)
  "Draw task details in read-only view mode."
  (let ((row y)
        (max-row (+ y height -1))
        (content-width (- width 2)))
    ;; Task title (bold, may wrap)
    (let ((title-lines (word-wrap (phitodo-tui.models:task-title task) content-width)))
      (dolist (line title-lines)
        (when (< row max-row)
          (move win row x)
          (setf (attributes win) '(:bold))
          (format win "~A" line)
          (setf (attributes win) '())
          (incf row))))
    (incf row)

    ;; Status line: Status, Priority, Kind, Size
    (when (< row max-row)
      (move win row x)
      (format win "Status: ~A"
              (string-downcase (symbol-name (phitodo-tui.models:task-status task))))
      ;; Priority (only if not :none)
      (when (not (eq (phitodo-tui.models:task-priority task) :none))
        (format win "  Priority: ~A"
                (string-downcase (symbol-name (phitodo-tui.models:task-priority task)))))
      (incf row))

    ;; Kind and Size on next line
    (when (< row max-row)
      (move win row x)
      (let ((has-kind (phitodo-tui.models:task-kind task))
            (has-size (phitodo-tui.models:task-size task)))
        (when has-kind
          (format win "Kind: ~A"
                  (phitodo-tui.models:kind-string has-kind)))
        (when has-size
          (when has-kind (format win "  "))
          (format win "Size: ~A"
                  (phitodo-tui.models:size-display has-size)))
        (when (or has-kind has-size)
          (incf row))))
    (incf row)

    ;; Notes section
    (when (< row max-row)
      (move win row x)
      (setf (attributes win) '(:underline))
      (format win "Notes:")
      (setf (attributes win) '())
      (incf row))

    (if (phitodo-tui.models:task-notes task)
        ;; Wrap and display notes
        (let ((note-lines (word-wrap (phitodo-tui.models:task-notes task) content-width)))
          (dolist (line note-lines)
            (when (< row max-row)
              (move win row x)
              (setf (color-pair win) '(:black :white))
              (format win "~A" line)
              (incf row))))
        ;; No notes
        (when (< row max-row)
          (move win row x)
          (setf (color-pair win) '(:black :white))
          (format win "No notes")
          (incf row)))
    (incf row)

    ;; Metadata section
    ;; Due date
    (when (and (< row max-row) (phitodo-tui.models:task-due-date task))
      (move win row x)
      (if (phitodo-tui.models:task-overdue-p task)
          (setf (color-pair win) '(:red :white))
          (setf (color-pair win) '(:black :white)))
      (format win "Due: ~A" (format-date (phitodo-tui.models:task-due-date task)))
      (setf (color-pair win) '(:black :white))
      (incf row))

    ;; Created/Updated
    (when (< row max-row)
      (move win row x)
      (setf (color-pair win) '(:black :white))
      (format win "Created: ~A" (format-timestamp (phitodo-tui.models:task-created-at task)))
      (incf row))

    (when (< row max-row)
      (move win row x)
      (format win "Updated: ~A" (format-timestamp (phitodo-tui.models:task-updated-at task)))
      (incf row))

    ;; Context URL
    (when (and (< row max-row) (phitodo-tui.models:task-context-url task))
      (move win row x)
      (setf (color-pair win) '(:blue :white))
      (format win "URL: ~A" (truncate-string (phitodo-tui.models:task-context-url task)
                                              content-width))
      (setf (color-pair win) '(:black :white)))))

(defun draw-detail-edit-field (win row x label value focused is-text-field max-width)
  "Draw an editable field in the detail pane."
  ;; Clear the line first
  (move win row x)
  (setf (color-pair win) '(:black :white))
  (format win "~A" (make-string max-width :initial-element #\Space))

  ;; Draw label
  (move win row x)
  (if focused
      (setf (color-pair win) '(:white :blue))
      (setf (color-pair win) '(:black :white)))
  (format win "~A: " label)
  (setf (color-pair win) '(:black :white))

  ;; Calculate available space and truncate value
  (let* ((label-len (+ (length label) 2))
         (bracket-len (if is-text-field 0 4))
         (available-len (- max-width label-len bracket-len 1))
         (clean-value (sanitize-for-display value))
         (display-value (if (> (length clean-value) available-len)
                            (concatenate 'string
                                        (subseq clean-value 0 (max 0 (- available-len 3)))
                                        "...")
                            clean-value)))
    (if is-text-field
        (progn
          (when focused
            (setf (attributes win) '(:underline)))
          (format win "~A" (if (string= display-value "") (if focused "_" "") display-value))
          (when focused
            (format win "_"))
          (setf (attributes win) '()))
        ;; Select field
        (progn
          (when focused
            (setf (attributes win) '(:bold)))
          (format win "< ~A >" display-value)
          (setf (attributes win) '())))))

(defun draw-detail-edit-notes (win start-row x max-width max-lines focused notes)
  "Draw multi-line notes editor. Returns the number of rows used."
  ;; Draw label
  (move win start-row x)
  (if focused
      (setf (color-pair win) '(:white :blue))
      (setf (color-pair win) '(:black :white)))
  (format win "Notes:")
  (when focused
    (format win " (Enter=newline, Tab=next)"))
  (setf (color-pair win) '(:black :white))

  ;; Split notes into lines
  (let* ((all-lines (if (or (null notes) (string= notes ""))
                        (list "")
                        (uiop:split-string notes :separator '(#\Newline))))
         ;; When focused, show last lines (where we're typing); otherwise show first lines
         (start-idx (if focused
                        (max 0 (- (length all-lines) max-lines))
                        0))
         (lines (subseq all-lines start-idx (min (length all-lines) (+ start-idx max-lines))))
         (row (1+ start-row)))
    ;; Draw each line
    (loop for line in lines
          for line-num from 0 below max-lines
          do (progn
               (move win row x)
               ;; Clear the line first
               (setf (color-pair win) '(:black :white))
               (format win "~A" (make-string max-width :initial-element #\Space))
               (move win row x)
               ;; Draw the text
               (when focused
                 (setf (attributes win) '(:underline)))
               (format win "  ~A" (truncate-string line (- max-width 4)))
               (setf (attributes win) '())
               (incf row)))
    ;; If focused, show cursor on a new line if there's room
    (when (and focused (< (length lines) max-lines))
      (move win row x)
      (setf (color-pair win) '(:black :white))
      (format win "~A" (make-string max-width :initial-element #\Space))
      (move win row x)
      (setf (attributes win) '(:underline))
      (format win "  _")
      (setf (attributes win) '())
      (incf row))
    ;; Return rows used (label + content lines)
    (max 2 (1+ (min max-lines (1+ (length lines)))))))

(defun draw-task-detail-edit (win x y width height task)
  "Draw task details in edit mode with editable fields."
  (let ((row y)
        (field-width (- width 2))
        (notes-max-lines 4))
    ;; Title field (0)
    (draw-detail-edit-field win row x "Title" *task-form-title*
                            (= *task-form-field* 0) t field-width)
    (incf row 2)

    ;; Notes field (1) - multi-line
    (let ((notes-rows (draw-detail-edit-notes win row x field-width notes-max-lines
                                               (= *task-form-field* 1) *task-form-notes*)))
      (incf row (1+ notes-rows)))

    ;; Due Date field (2)
    (draw-detail-edit-field win row x "Due Date" *task-form-due-date*
                            (= *task-form-field* 2) t field-width)
    (incf row 2)

    ;; Project field (3)
    (draw-detail-edit-field win row x "Project"
                            (get-project-name-by-id
                             (phitodo-tui.models:task-project-id task))
                            (= *task-form-field* 3) nil field-width)
    (incf row)

    ;; Priority field (4)
    (draw-detail-edit-field win row x "Priority"
                            (string-downcase
                             (symbol-name (phitodo-tui.models:task-priority task)))
                            (= *task-form-field* 4) nil field-width)
    (incf row)

    ;; Status field (5)
    (draw-detail-edit-field win row x "Status"
                            (string-downcase
                             (symbol-name (phitodo-tui.models:task-status task)))
                            (= *task-form-field* 5) nil field-width)
    (incf row)

    ;; Kind field (6)
    (draw-detail-edit-field win row x "Kind"
                            (if (phitodo-tui.models:task-kind task)
                                (phitodo-tui.models:kind-string
                                 (phitodo-tui.models:task-kind task))
                                "none")
                            (= *task-form-field* 6) nil field-width)
    (incf row)

    ;; Size field (7)
    (draw-detail-edit-field win row x "Size"
                            (if (phitodo-tui.models:task-size task)
                                (phitodo-tui.models:size-display
                                 (phitodo-tui.models:task-size task))
                                "none")
                            (= *task-form-field* 7) nil field-width)
    (incf row 2)

    ;; Context URL (read-only)
    (when (phitodo-tui.models:task-context-url task)
      (move win row x)
      (setf (color-pair win) '(:blue :white))
      (format win "URL: ~A" (truncate-string (phitodo-tui.models:task-context-url task)
                                              field-width))
      (setf (color-pair win) '(:black :white))
      (incf row 2))

    ;; Help text
    (when (< row (+ y height -1))
      (move win row x)
      (setf (color-pair win) '(:black :white))
      (format win "Tab:Next  Enter:Save  Esc:Cancel"))))

(defun draw-task-detail (win x y width height task)
  "Draw task details panel (view or edit mode based on focus)."
  (let ((row y)
        (editing (eq *focus* :detail)))
    ;; Draw header/title
    (move win row x)
    (setf (attributes win) '(:bold))
    (if editing
        (progn
          (setf (color-pair win) '(:white :blue))
          (format win " Edit Task ")
          (setf (color-pair win) '(:black :white)))
        (format win "Task Details"))
    (setf (attributes win) '())
    (incf row 2)

    (unless task
      ;; No task selected
      (move win row x)
      (setf (color-pair win) '(:black :white))
      (format win "No task selected")
      (return-from draw-task-detail))

    ;; Draw either view or edit mode
    (if editing
        (draw-task-detail-edit win x row width (- height 2) task)
        (draw-task-detail-view win x row width (- height 2) task))))

;;; Component: Status Bar
(defun draw-status-bar (win y width)
  "Draw status bar at the bottom."
  (move win y 0)
  ;; Background (white text on blue)
  (setf (color-pair win) '(:white :blue))
  (format win "~A" (make-string width :initial-element #\Space))
  (move win y 0)

  ;; Keys - show different hints based on focus
  (let ((keys (cond
                ;; Detail pane - notes field has different hint
                ((and (eq *focus* :detail) (= *task-form-field* 1))
                 "Enter:Newline  Tab:Next Field  Esc:Cancel")
                ((eq *focus* :detail)
                 "Tab:Next Field  Enter:Save  Esc:Cancel")
                ((eq *focus* :sidebar)
                 "j/k:Nav  Enter:Select  Tab:List  ?:Help  q:Quit")
                (t ; :list focus
                 "j/k:Nav  Tab:Edit  n:New  e:Edit  Space:Done  d:Del  ?:Help  q:Quit"))))
    (format win "~A" keys))

  ;; Status message on the right
  (when *status-message*
    (let ((msg-len (length *status-message*)))
      (move win y (max 0 (- width msg-len 1)))
      (format win "~A" *status-message*)))

  ;; Reset colors
  (setf (color-pair win) '(:black :white)))

;;; Component: Help Overlay
(defun draw-help-overlay (win width height)
  "Draw help overlay."
  (let* ((help-width 50)
         (help-height 20)
         (start-x (max 0 (floor (- width help-width) 2)))
         (start-y (max 0 (floor (- height help-height) 2)))
         (row 0))
    ;; Draw border
    (loop for r from 0 below help-height do
          (move win (+ start-y r) start-x)
          (cond
            ((= r 0) ; Top border
             (format win "+~A+"
                     (make-string (- help-width 2) :initial-element #\-)))
            ((= r (1- help-height)) ; Bottom border
             (format win "+~A+"
                     (make-string (- help-width 2) :initial-element #\-)))
            (t ; Content rows
             (format win "|~A|"
                     (make-string (- help-width 2) :initial-element #\Space)))))

    ;; Title
    (move win (+ start-y 1) (+ start-x 2))
    (setf (attributes win) '(:bold))
    (format win "Keyboard Shortcuts")
    (setf (attributes win) '())

    ;; Bindings
    (setf row 3)
    (dolist (section *help-bindings*)
      (when (< row (- help-height 2))
        ;; Section header
        (move win (+ start-y row) (+ start-x 2))
        (setf (attributes win) '(:underline))
        (format win "~A" (car section))
        (setf (attributes win) '())
        (incf row)
        ;; Keys
        (dolist (binding (cdr section))
          (when (< row (- help-height 2))
            (move win (+ start-y row) (+ start-x 4))
            (format win "~8A ~A" (car binding) (cdr binding))
            (incf row)))
        (incf row)))))

;;; Component: Input Dialog
(defun draw-input-dialog (win width height prompt current-value)
  "Draw input dialog."
  (let* ((dialog-width 60)
         (dialog-height 5)
         (start-x (max 0 (floor (- width dialog-width) 2)))
         (start-y (max 0 (floor (- height dialog-height) 2))))
    ;; Draw border
    (loop for r from 0 below dialog-height do
          (move win (+ start-y r) start-x)
          (cond
            ((= r 0)
             (format win "+~A+"
                     (make-string (- dialog-width 2) :initial-element #\-)))
            ((= r (1- dialog-height))
             (format win "+~A+"
                     (make-string (- dialog-width 2) :initial-element #\-)))
            (t
             (format win "|~A|"
                     (make-string (- dialog-width 2) :initial-element #\Space)))))
    ;; Prompt
    (move win (+ start-y 1) (+ start-x 2))
    (setf (attributes win) '(:bold))
    (format win "~A" prompt)
    (setf (attributes win) '())
    ;; Input field
    (move win (+ start-y 2) (+ start-x 2))
    (format win "~A" current-value)
    (setf (attributes win) '(:blink))
    (format win "_")
    (setf (attributes win) '())))

;;; Component: Task Form
(defun sanitize-for-display (str)
  "Remove newlines and extra whitespace for single-line display."
  (if (or (null str) (string= str ""))
      ""
      (substitute #\Space #\Newline
                  (substitute #\Space #\Return str))))

(defun draw-task-form-field (win row x label value focused is-text-field max-value-len)
  "Draw a single form field at the given row."
  ;; Sanitize value - remove newlines for single-line display
  (let ((clean-value (sanitize-for-display value)))
    ;; First clear the entire field area
    (move win row x)
    (setf (color-pair win) '(:black :white))
    (format win "~A" (make-string max-value-len :initial-element #\Space))

    ;; Now draw the actual field content
    (move win row x)
    (if focused
        (setf (color-pair win) '(:blue :white))
        (setf (color-pair win) '(:black :white)))
    (format win "~A: " label)
    (setf (color-pair win) '(:black :white))

    ;; Calculate available length for value (account for brackets in select fields)
    (let* ((label-len (+ (length label) 2)) ; "Label: "
           (bracket-len (if is-text-field 0 4)) ; "< " and " >"
           (available-len (- max-value-len label-len bracket-len 1))
           (display-value (if (> (length clean-value) available-len)
                              (concatenate 'string
                                          (subseq clean-value 0 (max 0 (- available-len 3)))
                                          "...")
                              clean-value)))
    (if is-text-field
        (progn
          (when focused
            (setf (attributes win) '(:underline)))
          (format win "~A" (if (string= display-value "") (if focused "_" "") display-value))
          (setf (attributes win) '()))
        ;; Select field
        (progn
          (when focused
            (setf (attributes win) '(:bold)))
          (format win "< ~A >" display-value)
          (setf (attributes win) '()))))))

(defun get-project-name-by-id (project-id)
  "Get project name by ID."
  (if project-id
      (let ((project (find project-id *projects*
                           :key #'phitodo-tui.models:project-id
                           :test #'equal)))
        (if project
            (phitodo-tui.models:project-name project)
            "none"))
      "none"))

(defun draw-task-form (win width height)
  "Draw the task form dialog."
  (let* ((form-width 60)
         (form-height 18)
         (start-x (max 0 (floor (- width form-width) 2)))
         (start-y (max 0 (floor (- height form-height) 2)))
         (inner-x (+ start-x 2))
         (task *task-form-task*))
    ;; Draw border
    (loop for r from 0 below form-height do
          (move win (+ start-y r) start-x)
          (cond
            ((= r 0)
             (format win "+~A+"
                     (make-string (- form-width 2) :initial-element #\-)))
            ((= r (1- form-height))
             (format win "+~A+"
                     (make-string (- form-width 2) :initial-element #\-)))
            (t
             (format win "|~A|"
                     (make-string (- form-width 2) :initial-element #\Space)))))

    ;; Dialog title
    (move win (+ start-y 1) inner-x)
    (setf (attributes win) '(:bold))
    (format win "~A" (if *task-form-is-new* "New Task" "Edit Task"))
    (setf (attributes win) '())

    ;; Fields - each on its own row
    (let ((field-width (- form-width 6))) ; Width available for field content
      ;; Row 3: Title
      (draw-task-form-field win (+ start-y 3) inner-x
                            "Title" *task-form-title*
                            (= *task-form-field* 0) t field-width)
      ;; Row 5: Notes
      (draw-task-form-field win (+ start-y 5) inner-x
                            "Notes" *task-form-notes*
                            (= *task-form-field* 1) t field-width)
      ;; Row 7: Due Date
      (draw-task-form-field win (+ start-y 7) inner-x
                            "Due Date" *task-form-due-date*
                            (= *task-form-field* 2) t field-width)
      ;; Row 9: Project
      (draw-task-form-field win (+ start-y 9) inner-x
                            "Project"
                            (get-project-name-by-id
                             (phitodo-tui.models:task-project-id task))
                            (= *task-form-field* 3) nil field-width)
      ;; Row 10: Priority
      (draw-task-form-field win (+ start-y 10) inner-x
                            "Priority"
                            (string-downcase
                             (symbol-name (phitodo-tui.models:task-priority task)))
                            (= *task-form-field* 4) nil field-width)
      ;; Row 11: Status
      (draw-task-form-field win (+ start-y 11) inner-x
                            "Status"
                            (string-downcase
                             (symbol-name (phitodo-tui.models:task-status task)))
                            (= *task-form-field* 5) nil field-width)
      ;; Row 12: Kind
      (draw-task-form-field win (+ start-y 12) inner-x
                            "Kind"
                            (if (phitodo-tui.models:task-kind task)
                                (phitodo-tui.models:kind-string
                                 (phitodo-tui.models:task-kind task))
                                "none")
                            (= *task-form-field* 6) nil field-width)
      ;; Row 13: Size
      (draw-task-form-field win (+ start-y 13) inner-x
                            "Size"
                            (if (phitodo-tui.models:task-size task)
                                (phitodo-tui.models:size-display
                                 (phitodo-tui.models:task-size task))
                                "none")
                            (= *task-form-field* 7) nil field-width))

    ;; Help text at bottom
    (move win (+ start-y form-height -2) inner-x)
    (setf (color-pair win) '(:black :white))
    (format win "Tab:Next  Enter:Save  Esc:Cancel  Space:Cycle")))
