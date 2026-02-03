;;;; app.lisp - Main application logic

(in-package #:phitodo-tui.ui)

;;; Application state
(defvar *running* t)
(defvar *config* nil)
(defvar *input-mode* nil)
(defvar *input-buffer* "")
(defvar *input-prompt* "")
(defvar *input-callback* nil)

;;; Layout constants
(defparameter *sidebar-width* 20)

;;; Initialize application
(defun init-app ()
  "Initialize application state."
  (setf *running* t)
  (setf *current-view* :inbox)
  (setf *selected-index* 0)
  (setf *focus* :sidebar)
  (setf *show-help* nil)
  (setf *status-message* nil)
  (setf *input-mode* nil)
  (setf *input-buffer* "")
  (reload-data))

(defun reload-data ()
  "Reload all data from database."
  (setf *tasks* (phitodo-tui.db:get-all-tasks))
  (setf *projects* (phitodo-tui.db:get-all-projects))
  (setf *tags* (phitodo-tui.db:get-all-tags))
  (setf *status-message* "Data loaded"))

;;; Navigation
(defun switch-view (view)
  "Switch to a different view."
  (setf *current-view* view)
  (setf *selected-index* 0)
  (setf *focus* :list)
  (setf *status-message* (format nil "View: ~A" (sidebar-item-label view))))

(defvar *sidebar-index* 0 "Current sidebar selection index")
(defvar *settings-selected* 0 "Current settings field index")

(defun move-selection (delta)
  "Move selection by delta."
  (cond
    ;; Move in sidebar
    ((eq *focus* :sidebar)
     (let ((num-items (length *sidebar-items*)))
       (setf *sidebar-index*
             (max 0 (min (1- num-items)
                         (+ *sidebar-index* delta))))))
    ;; Move in settings view
    ((eq *current-view* :settings)
     (let ((num-fields 3)) ; db-path, github-token, toggl-token
       (setf *settings-selected*
             (max 0 (min (1- num-fields)
                         (+ *settings-selected* delta))))))
    ;; Move in task list
    (t
     (let ((tasks (get-current-tasks)))
       (when tasks
         (setf *selected-index*
               (max 0 (min (1- (length tasks))
                           (+ *selected-index* delta)))))))))

(defun move-to-start ()
  "Move selection to first item."
  (setf *selected-index* 0))

(defun move-to-end ()
  "Move selection to last item."
  (let ((tasks (get-current-tasks)))
    (when tasks
      (setf *selected-index* (1- (length tasks))))))

(defun toggle-focus ()
  "Toggle focus between sidebar and list."
  (setf *focus* (if (eq *focus* :sidebar) :list :sidebar)))

;;; Task actions
(defun toggle-task-complete ()
  "Toggle completion status of selected task."
  (let ((task (get-selected-task)))
    (when task
      (if (phitodo-tui.models:task-completed-p task)
          (progn
            (setf (phitodo-tui.models:task-status task) :active)
            (setf (phitodo-tui.models:task-completed-at task) nil))
          (phitodo-tui.models:complete-task task))
      (phitodo-tui.db:update-task task)
      (reload-data)
      (setf *status-message* "Task updated"))))

(defun delete-selected-task ()
  "Delete the selected task."
  (let ((task (get-selected-task)))
    (when task
      (phitodo-tui.db:delete-task (phitodo-tui.models:task-id task))
      (reload-data)
      (setf *status-message* "Task deleted")
      ;; Adjust selection if needed
      (let ((tasks (get-current-tasks)))
        (when (>= *selected-index* (length tasks))
          (setf *selected-index* (max 0 (1- (length tasks)))))))))

(defun open-task-url ()
  "Open URL of selected task."
  (let ((task (get-selected-task)))
    (when (and task (phitodo-tui.models:task-context-url task))
      #+sbcl (sb-ext:run-program "open"
                                  (list (phitodo-tui.models:task-context-url task))
                                  :wait nil)
      (setf *status-message* "Opening URL..."))))

(defun set-task-priority (priority)
  "Set priority of selected task."
  (let ((task (get-selected-task)))
    (when task
      (setf (phitodo-tui.models:task-priority task) priority)
      (phitodo-tui.db:update-task task)
      (reload-data)
      (setf *status-message* (format nil "Priority: ~A" priority)))))

;;; Input handling
(defun start-input (prompt callback &optional initial-value)
  "Start input mode with optional initial value."
  (setf *input-mode* t)
  (setf *input-prompt* prompt)
  (setf *input-buffer* (or initial-value ""))
  (setf *input-callback* callback))

(defun finish-input ()
  "Finish input mode and call callback."
  (setf *input-mode* nil)
  (when *input-callback*
    (funcall *input-callback* *input-buffer*))
  (setf *input-buffer* "")
  (setf *input-callback* nil))

(defun cancel-input ()
  "Cancel input mode."
  (setf *input-mode* nil)
  (setf *input-buffer* "")
  (setf *input-callback* nil))

(defun new-task-action (title)
  "Create a new task with the given title."
  (when (and title (> (length title) 0))
    (let ((task (phitodo-tui.models:make-task title)))
      (phitodo-tui.db:insert-task task)
      (reload-data)
      (setf *status-message* "Task created"))))

(defun start-new-task ()
  "Start creating a new task with full form."
  (setf *task-form-mode* t)
  (setf *task-form-is-new* t)
  (setf *task-form-task* (phitodo-tui.models:make-task ""))
  (setf *task-form-field* 0)
  (setf *task-form-title* "")
  (setf *task-form-notes* "")
  (setf *task-form-due-date* ""))

(defun start-edit-task ()
  "Start editing the selected task with full form."
  (let ((task (get-selected-task)))
    (if task
        (progn
          (setf *task-form-mode* t)
          (setf *task-form-is-new* nil)
          (setf *task-form-task* task)
          (setf *task-form-field* 0)
          (setf *task-form-title* (or (phitodo-tui.models:task-title task) ""))
          (setf *task-form-notes* (or (phitodo-tui.models:task-notes task) ""))
          (setf *task-form-due-date*
                (if (phitodo-tui.models:task-due-date task)
                    (local-time:format-timestring
                     nil (phitodo-tui.models:task-due-date task)
                     :format '(:year #\- (:month 2) #\- (:day 2)))
                    "")))
        (setf *status-message* "No task selected"))))

(defun save-task-form ()
  "Save the task from the form."
  (let ((task *task-form-task*))
    ;; Apply text inputs
    (setf (phitodo-tui.models:task-title task) *task-form-title*)
    (setf (phitodo-tui.models:task-notes task)
          (if (string= *task-form-notes* "") nil *task-form-notes*))
    (setf (phitodo-tui.models:task-due-date task)
          (when (and *task-form-due-date* (not (string= *task-form-due-date* "")))
            (handler-case
                (local-time:parse-timestring *task-form-due-date* :date-separator #\-)
              (error () nil))))
    ;; Save to database
    (if *task-form-is-new*
        (phitodo-tui.db:insert-task task)
        (phitodo-tui.db:update-task task))
    (reload-data)
    (setf *task-form-mode* nil)
    (setf *status-message* (if *task-form-is-new* "Task created" "Task updated"))))

(defun cancel-task-form ()
  "Cancel the task form."
  (setf *task-form-mode* nil)
  (setf *status-message* "Cancelled"))

(defun task-form-next-field ()
  "Move to next field in task form."
  (setf *task-form-field* (mod (1+ *task-form-field*) 8)))

(defun task-form-prev-field ()
  "Move to previous field in task form."
  (setf *task-form-field* (mod (+ *task-form-field* 7) 8)))

(defun task-form-cycle-value ()
  "Cycle the value of the current select field."
  (let ((task *task-form-task*))
    (case *task-form-field*
      (3 ; Project
       (let* ((current-id (phitodo-tui.models:task-project-id task))
              (current-idx (when current-id
                            (position current-id *projects*
                                      :key #'phitodo-tui.models:project-id
                                      :test #'equal)))
              (next-idx (if current-idx
                           (if (< (1+ current-idx) (length *projects*))
                               (1+ current-idx)
                               nil)
                           (when *projects* 0))))
         (setf (phitodo-tui.models:task-project-id task)
               (when next-idx
                 (phitodo-tui.models:project-id (nth next-idx *projects*))))))
      (4 ; Priority
       (setf (phitodo-tui.models:task-priority task)
             (case (phitodo-tui.models:task-priority task)
               (:none :low)
               (:low :medium)
               (:medium :high)
               (:high :none)
               (otherwise :none))))
      (5 ; Status
       (setf (phitodo-tui.models:task-status task)
             (case (phitodo-tui.models:task-status task)
               (:inbox :active)
               (:active :scheduled)
               (:scheduled :completed)
               (:completed :cancelled)
               (:cancelled :inbox)
               (otherwise :inbox))))
      (6 ; Kind
       (setf (phitodo-tui.models:task-kind task)
             (case (phitodo-tui.models:task-kind task)
               ((nil) :task)
               (:task :bug)
               (:bug :feature)
               (:feature :chore)
               (:chore :gh-issue)
               (:gh-issue :gh-pr)
               (:gh-pr :gh-review)
               (:gh-review nil)
               (otherwise nil))))
      (7 ; Size
       (setf (phitodo-tui.models:task-size task)
             (case (phitodo-tui.models:task-size task)
               ((nil) :xs)
               (:xs :s)
               (:s :m)
               (:m :l)
               (:l nil)
               (otherwise nil)))))))

(defun edit-setting-action (new-value)
  "Update the selected setting."
  (case *settings-selected*
    (0 ; db-path
     (setf (phitodo-tui.config:config-db-path *config*) new-value))
    (1 ; github-token
     (setf (phitodo-tui.config:config-github-token *config*) new-value))
    (2 ; toggl-token
     (setf (phitodo-tui.config:config-toggl-token *config*) new-value)))
  (phitodo-tui.config:save-config)
  (setf *status-message* "Setting saved"))

(defun start-edit-setting ()
  "Start editing the selected setting."
  (let* ((prompts '("Database path: " "GitHub token: " "Toggl token: "))
         (current-values (list (or (phitodo-tui.config:config-db-path *config*) "")
                               (or (phitodo-tui.config:config-github-token *config*) "")
                               (or (phitodo-tui.config:config-toggl-token *config*) ""))))
    (start-input (nth *settings-selected* prompts)
                 #'edit-setting-action
                 (nth *settings-selected* current-values))))

;;; Key event handling
(defun handle-input-key (event)
  "Handle key in input mode."
  (cond
    ((eql event #\Newline)
     (finish-input))
    ((eql event #\Return)
     (finish-input))
    ((or (eql event :escape)
         (eql event #\Escape))
     (cancel-input))
    ((or (eql event #\Backspace)
         (eql event #\Rubout)
         (eql event :backspace))
     (when (> (length *input-buffer*) 0)
       (setf *input-buffer* (subseq *input-buffer* 0 (1- (length *input-buffer*))))))
    ((characterp event)
     (when (graphic-char-p event)
       (setf *input-buffer* (concatenate 'string *input-buffer* (string event)))))))

(defun handle-normal-key (event)
  "Handle key in normal mode."
  (cond
    ;; Quit
    ((eql event #\q)
     (setf *running* nil))

    ;; Help toggle
    ((eql event #\?)
     (setf *show-help* (not *show-help*)))

    ;; Close help with escape
    ((and *show-help* (or (eql event :escape) (eql event #\Escape)))
     (setf *show-help* nil))

    ;; View switching (1-9)
    ((eql event #\1) (switch-view :inbox))
    ((eql event #\2) (switch-view :today))
    ((eql event #\3) (switch-view :upcoming))
    ((eql event #\4) (switch-view :anytime))
    ((eql event #\5) (switch-view :completed))
    ((eql event #\6) (switch-view :review))
    ((eql event #\7) (switch-view :github))
    ((eql event #\8) (switch-view :toggl))
    ((eql event #\9) (switch-view :settings))

    ;; Navigation
    ((or (eql event #\j) (eql event :down) (eql event :key-down))
     (move-selection 1))
    ((or (eql event #\k) (eql event :up) (eql event :key-up))
     (move-selection -1))
    ((eql event #\g)
     (move-to-start))
    ((eql event #\G)
     (move-to-end))
    ((or (eql event #\Tab) (eql event #\h) (eql event #\l)
         (eql event :left) (eql event :right)
         (eql event :key-left) (eql event :key-right))
     (toggle-focus))

    ;; Enter to activate
    ((or (eql event #\Return) (eql event #\Newline) (eql event :enter))
     (cond
       ;; Sidebar: activate selected item
       ((eq *focus* :sidebar)
        (let ((item (nth *sidebar-index* *sidebar-items*)))
          (switch-view item)))
       ;; Settings: edit selected field
       ((eq *current-view* :settings)
        (start-edit-setting))
       ;; Task list: placeholder
       (t
        (setf *status-message* "Task detail view not implemented"))))

    ;; Task actions
    ((eql event #\Space)
     (toggle-task-complete))
    ((eql event #\d)
     (delete-selected-task))
    ((eql event #\e)
     (start-edit-task))
    ((eql event #\o)
     (open-task-url))
    ((eql event #\n)
     (start-new-task))
    ((eql event #\r)
     (if (eq *current-view* :github)
         (refresh-github)
         (reload-data)))))

(defun refresh-github ()
  "Refresh data from GitHub and sync to tasks."
  (let ((token (phitodo-tui.config:config-github-token *config*)))
    (if token
        (progn
          (setf *status-message* "Fetching from GitHub...")
          (multiple-value-bind (issues prs reviews error)
              (phitodo-tui.services:sync-all-github token)
            (if error
                (setf *status-message* (format nil "GitHub: ~A" error))
                (progn
                  ;; Store for display
                  (setf *github-issues* issues)
                  (setf *github-prs* prs)
                  (setf *github-reviews* reviews)
                  ;; Sync to database as tasks
                  (multiple-value-bind (created updated completed)
                      (phitodo-tui.services:sync-github-to-tasks issues prs reviews)
                    ;; Reload tasks from database
                    (reload-data)
                    (setf *status-message*
                          (format nil "GitHub: +~A tasks, ~A updated, ~A completed"
                                  created updated completed)))))))
        (setf *status-message* "No GitHub token configured"))))

(defun get-key-name (key)
  "Extract the key name from a key object or return the key itself."
  (if (and (not (characterp key))
           (slot-exists-p key 'name))
      (slot-value key 'name)
      key))

(defun handle-task-form-key (key)
  "Handle key in task form mode."
  (cond
    ;; Escape - cancel
    ((or (eql key #\Escape) (eql key :escape))
     (cancel-task-form))
    ;; Enter - save
    ((or (eql key #\Return) (eql key #\Newline) (eql key :enter))
     (save-task-form))
    ;; Tab - next field
    ((eql key #\Tab)
     (task-form-next-field))
    ;; Shift+Tab or backtab - previous field (simplified: use k)
    ((or (eql key :btab) (eql key :backtab))
     (task-form-prev-field))
    ;; j/down - next field
    ((or (eql key #\j) (eql key :down) (eql key :key-down))
     (task-form-next-field))
    ;; k/up - previous field
    ((or (eql key #\k) (eql key :up) (eql key :key-up))
     (task-form-prev-field))
    ;; Space - cycle value for select fields
    ((eql key #\Space)
     (if (<= *task-form-field* 2)
         ;; Text field - add space
         (case *task-form-field*
           (0 (setf *task-form-title* (concatenate 'string *task-form-title* " ")))
           (1 (setf *task-form-notes* (concatenate 'string *task-form-notes* " ")))
           (2 nil)) ; No space in date
         ;; Select field - cycle
         (task-form-cycle-value)))
    ;; Left/Right - cycle value for select fields
    ((or (eql key #\l) (eql key :right) (eql key :key-right)
         (eql key #\h) (eql key :left) (eql key :key-left))
     (when (> *task-form-field* 2)
       (task-form-cycle-value)))
    ;; Backspace
    ((or (eql key #\Backspace) (eql key #\Rubout) (eql key :backspace))
     (case *task-form-field*
       (0 (when (> (length *task-form-title*) 0)
            (setf *task-form-title* (subseq *task-form-title* 0 (1- (length *task-form-title*))))))
       (1 (when (> (length *task-form-notes*) 0)
            (setf *task-form-notes* (subseq *task-form-notes* 0 (1- (length *task-form-notes*))))))
       (2 (when (> (length *task-form-due-date*) 0)
            (setf *task-form-due-date* (subseq *task-form-due-date* 0 (1- (length *task-form-due-date*))))))))
    ;; Character input for text fields
    ((and (characterp key) (graphic-char-p key) (<= *task-form-field* 2))
     (case *task-form-field*
       (0 (setf *task-form-title* (concatenate 'string *task-form-title* (string key))))
       (1 (setf *task-form-notes* (concatenate 'string *task-form-notes* (string key))))
       (2 (setf *task-form-due-date* (concatenate 'string *task-form-due-date* (string key))))))))

(defun handle-event (event)
  "Handle keyboard input."
  (when event
    ;; Extract key from event object
    (let* ((raw-key (if (typep event 'event)
                        (event-key event)
                        event))
           (key (get-key-name raw-key)))
      (cond
        (*task-form-mode*
         (handle-task-form-key key))
        (*input-mode*
         (handle-input-key key))
        (t
         (handle-normal-key key))))))

;;; Rendering
(defun render (win)
  "Render the entire UI."
  (clear win)
  (let ((width (width win))
        (height (height win)))

    ;; Sidebar
    (draw-sidebar win 0 0 *sidebar-width* (- height 1))

    ;; Main content area
    (let ((content-x (+ *sidebar-width* 1))
          (content-width (- width *sidebar-width* 1))
          (content-height (- height 1)))
      (render-current-view win content-x 0 content-width content-height *config*))

    ;; Status bar
    (draw-status-bar win (- height 1) width)

    ;; Help overlay
    (when *show-help*
      (draw-help-overlay win width height))

    ;; Input dialog
    (when *input-mode*
      (draw-input-dialog win width height *input-prompt* *input-buffer*))

    ;; Task form
    (when *task-form-mode*
      (draw-task-form win width height)))

  (refresh win))

;;; Main loop
(defun run-app ()
  "Run the main application loop."
  (with-screen (scr :input-echoing nil
                    :input-blocking t
                    :enable-colors t
                    :cursor-visible nil
                    :enable-function-keys t)
    (setf *screen* scr)
    (setf *running* t)

    (init-app)

    ;; Set default colors (black on white)
    (setf (background scr) (make-instance 'complex-char :simple-char #\Space
                                          :color-pair '(:black :white)))
    (clear scr)

    ;; Initial render before entering event loop
    (render scr)

    ;; Main event loop
    (loop while *running* do
      (let ((event (get-event scr)))
        (when event
          (handle-event event)
          (render scr))))))

;;; Entry point
(defun start (&optional config-path)
  "Start the application."
  ;; Load config
  (setf *config* (if config-path
                     (phitodo-tui.config:load-config config-path)
                     (phitodo-tui.config:load-config)))

  ;; Connect to database
  (let ((db-path (or (phitodo-tui.config:config-db-path *config*)
                     (namestring (phitodo-tui.config:default-db-path)))))
    (phitodo-tui.db:connect-db db-path))

  (unwind-protect
       (run-app)
    ;; Cleanup
    (phitodo-tui.db:disconnect-db)))
