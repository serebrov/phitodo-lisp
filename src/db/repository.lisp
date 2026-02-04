;;;; repository.lisp - Database access layer

(in-package #:phitodo-tui.db)

(defvar *db* nil "Current database connection")

(defun connect-db (path)
  "Connect to SQLite database at PATH."
  (setf *db* (dbi:connect :sqlite3 :database-name path))
  (init-schema *db*)
  *db*)

(defun disconnect-db ()
  "Disconnect from the database."
  (when *db*
    (dbi:disconnect *db*)
    (setf *db* nil)))

(defmacro with-db ((db-var path) &body body)
  "Execute BODY with DB-VAR bound to a database connection."
  `(let ((,db-var (dbi:connect :sqlite3 :database-name ,path)))
     (unwind-protect
          (progn
            (init-schema ,db-var)
            ,@body)
       (dbi:disconnect ,db-var))))

;;; Timestamp utilities
(defun timestamp-to-string (ts)
  "Convert local-time timestamp to ISO string."
  (when ts
    (local-time:format-timestring nil ts :format local-time:+iso-8601-format+)))

(defun string-to-timestamp (str)
  "Convert ISO string to local-time timestamp."
  (when (and str (not (string= str "")))
    (local-time:parse-timestring str)))

(defun date-to-string (ts)
  "Convert local-time timestamp to date string (YYYY-MM-DD) in UTC."
  (when ts
    (local-time:format-timestring nil ts
                                  :format '(:year #\- (:month 2) #\- (:day 2))
                                  :timezone local-time:+utc-zone+)))

(defun string-to-date (str)
  "Convert date string to local-time timestamp at midnight UTC."
  (when (and str (not (string= str "")))
    (handler-case
        (let ((parts (uiop:split-string str :separator '(#\-))))
          (when (= (length parts) 3)
            (local-time:encode-timestamp
             0 0 0 0  ; nsec sec min hour
             (parse-integer (third parts))   ; day
             (parse-integer (second parts))  ; month
             (parse-integer (first parts))   ; year
             :timezone local-time:+utc-zone+)))
      (error () nil))))

;;; Task Operations

(defun row-to-task (row)
  "Convert database row to task object."
  (let ((task (make-instance 'task
                             :id (getf row :|id|)
                             :title (getf row :|title|)
                             :notes (getf row :|notes|)
                             :created-at (string-to-timestamp (getf row :|created_at|))
                             :updated-at (string-to-timestamp (getf row :|updated_at|))
                             :due-date (string-to-date (getf row :|due_date|))
                             :start-date (string-to-date (getf row :|start_date|))
                             :completed-at (string-to-timestamp (getf row :|completed_at|))
                             :project-id (getf row :|project_id|)
                             :priority (string-priority (getf row :|priority|))
                             :status (string-task-status (getf row :|status|))
                             :order-index (or (getf row :|order_index|) 0)
                             :deleted-p (= 1 (or (getf row :|deleted|) 0))
                             :kind (string-kind (getf row :|kind|))
                             :size (string-size (getf row :|size|))
                             :assignee (getf row :|assignee|)
                             :context-url (getf row :|context_url|))))
    ;; Parse metadata JSON if present
    (let ((metadata-str (getf row :|metadata|)))
      (when (and metadata-str (not (string= metadata-str "")))
        (let ((ht (make-hash-table :test 'equal)))
          (handler-case
              (let ((parsed (yason:parse metadata-str)))
                (maphash (lambda (k v) (setf (gethash k ht) v)) parsed))
            (error () nil))
          (setf (task-metadata task) ht))))
    task))

(defun get-all-tasks ()
  "Get all non-deleted tasks."
  (let* ((query (dbi:prepare *db*
                             "SELECT * FROM tasks WHERE deleted = 0 ORDER BY order_index, created_at"))
         (result (dbi:execute query)))
    (loop for row = (dbi:fetch result)
          while row
          collect (row-to-task row))))

(defun get-task (id)
  "Get a task by ID."
  (let* ((query (dbi:prepare *db*
                             "SELECT * FROM tasks WHERE id = ? AND deleted = 0"))
         (result (dbi:execute query (list id)))
         (row (dbi:fetch result)))
    (when row
      (row-to-task row))))

(defun insert-task (task)
  "Insert a new task."
  (let ((metadata-json (with-output-to-string (s)
                         (yason:encode (task-metadata task) s))))
    (dbi:do-sql *db*
                "INSERT INTO tasks (id, title, notes, created_at, updated_at, due_date, start_date, completed_at, project_id, priority, status, order_index, deleted, kind, size, assignee, context_url, metadata)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                (list (task-id task)
                      (task-title task)
                      (task-notes task)
                      (timestamp-to-string (task-created-at task))
                      (timestamp-to-string (task-updated-at task))
                      (date-to-string (task-due-date task))
                      (date-to-string (task-start-date task))
                      (timestamp-to-string (task-completed-at task))
                      (task-project-id task)
                      (priority-string (task-priority task))
                      (task-status-string (task-status task))
                      (task-order-index task)
                      (if (task-deleted-p task) 1 0)
                      (kind-string (task-kind task))
                      (size-string (task-size task))
                      (task-assignee task)
                      (task-context-url task)
                      metadata-json))))

(defun update-task (task)
  "Update an existing task."
  (setf (task-updated-at task) (local-time:now))
  (let ((metadata-json (with-output-to-string (s)
                         (yason:encode (task-metadata task) s))))
    (dbi:do-sql *db*
                "UPDATE tasks SET title=?, notes=?, updated_at=?, due_date=?, start_date=?, completed_at=?, project_id=?, priority=?, status=?, order_index=?, deleted=?, kind=?, size=?, assignee=?, context_url=?, metadata=? WHERE id=?"
                (list (task-title task)
                      (task-notes task)
                      (timestamp-to-string (task-updated-at task))
                      (date-to-string (task-due-date task))
                      (date-to-string (task-start-date task))
                      (timestamp-to-string (task-completed-at task))
                      (task-project-id task)
                      (priority-string (task-priority task))
                      (task-status-string (task-status task))
                      (task-order-index task)
                      (if (task-deleted-p task) 1 0)
                      (kind-string (task-kind task))
                      (size-string (task-size task))
                      (task-assignee task)
                      (task-context-url task)
                      metadata-json
                      (task-id task)))))

(defun delete-task (id)
  "Soft delete a task."
  (dbi:do-sql *db*
              "UPDATE tasks SET deleted = 1, updated_at = ? WHERE id = ?"
              (list (timestamp-to-string (local-time:now)) id)))

;;; Project Operations

(defun row-to-project (row)
  "Convert database row to project object."
  (make-instance 'project
                 :id (getf row :|id|)
                 :name (getf row :|name|)
                 :description (getf row :|description|)
                 :color (getf row :|color|)
                 :icon (getf row :|icon|)
                 :order-index (or (getf row :|order_index|) 0)
                 :inbox-p (= 1 (or (getf row :|is_inbox|) 0))
                 :created-at (string-to-timestamp (getf row :|created_at|))
                 :updated-at (string-to-timestamp (getf row :|updated_at|))
                 :deleted-p (= 1 (or (getf row :|deleted|) 0))))

(defun get-all-projects ()
  "Get all non-deleted projects."
  (let* ((query (dbi:prepare *db*
                             "SELECT * FROM projects WHERE deleted = 0 ORDER BY order_index, name"))
         (result (dbi:execute query)))
    (loop for row = (dbi:fetch result)
          while row
          collect (row-to-project row))))

(defun get-project (id)
  "Get a project by ID."
  (let* ((query (dbi:prepare *db*
                             "SELECT * FROM projects WHERE id = ? AND deleted = 0"))
         (result (dbi:execute query (list id)))
         (row (dbi:fetch result)))
    (when row
      (row-to-project row))))

(defun insert-project (project)
  "Insert a new project."
  (dbi:do-sql *db*
              "INSERT INTO projects (id, name, description, color, icon, order_index, is_inbox, created_at, updated_at, deleted)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
              (list (project-id project)
                    (project-name project)
                    (project-description project)
                    (project-color project)
                    (project-icon project)
                    (project-order-index project)
                    (if (project-inbox-p project) 1 0)
                    (timestamp-to-string (project-created-at project))
                    (timestamp-to-string (project-updated-at project))
                    (if (project-deleted-p project) 1 0))))

(defun update-project (project)
  "Update an existing project."
  (setf (project-updated-at project) (local-time:now))
  (dbi:do-sql *db*
              "UPDATE projects SET name=?, description=?, color=?, icon=?, order_index=?, is_inbox=?, updated_at=?, deleted=? WHERE id=?"
              (list (project-name project)
                    (project-description project)
                    (project-color project)
                    (project-icon project)
                    (project-order-index project)
                    (if (project-inbox-p project) 1 0)
                    (timestamp-to-string (project-updated-at project))
                    (if (project-deleted-p project) 1 0)
                    (project-id project))))

(defun delete-project (id)
  "Soft delete a project."
  (dbi:do-sql *db*
              "UPDATE projects SET deleted = 1, updated_at = ? WHERE id = ?"
              (list (timestamp-to-string (local-time:now)) id)))

;;; Tag Operations

(defun row-to-tag (row)
  "Convert database row to tag object."
  (make-instance 'tag
                 :id (getf row :|id|)
                 :name (getf row :|name|)
                 :color (getf row :|color|)
                 :created-at (string-to-timestamp (getf row :|created_at|))
                 :updated-at (string-to-timestamp (getf row :|updated_at|))
                 :deleted-p (= 1 (or (getf row :|deleted|) 0))))

(defun get-all-tags ()
  "Get all non-deleted tags."
  (let* ((query (dbi:prepare *db*
                             "SELECT * FROM tags WHERE deleted = 0 ORDER BY name"))
         (result (dbi:execute query)))
    (loop for row = (dbi:fetch result)
          while row
          collect (row-to-tag row))))

(defun get-tag (id)
  "Get a tag by ID."
  (let* ((query (dbi:prepare *db*
                             "SELECT * FROM tags WHERE id = ? AND deleted = 0"))
         (result (dbi:execute query (list id)))
         (row (dbi:fetch result)))
    (when row
      (row-to-tag row))))

(defun insert-tag (tag)
  "Insert a new tag."
  (dbi:do-sql *db*
              "INSERT INTO tags (id, name, color, created_at, updated_at, deleted)
               VALUES (?, ?, ?, ?, ?, ?)"
              (list (tag-id tag)
                    (tag-name tag)
                    (tag-color tag)
                    (timestamp-to-string (tag-created-at tag))
                    (timestamp-to-string (tag-updated-at tag))
                    (if (tag-deleted-p tag) 1 0))))

(defun update-tag (tag)
  "Update an existing tag."
  (setf (tag-updated-at tag) (local-time:now))
  (dbi:do-sql *db*
              "UPDATE tags SET name=?, color=?, updated_at=?, deleted=? WHERE id=?"
              (list (tag-name tag)
                    (tag-color tag)
                    (timestamp-to-string (tag-updated-at tag))
                    (if (tag-deleted-p tag) 1 0)
                    (tag-id tag))))

(defun delete-tag (id)
  "Soft delete a tag."
  (dbi:do-sql *db*
              "UPDATE tags SET deleted = 1, updated_at = ? WHERE id = ?"
              (list (timestamp-to-string (local-time:now)) id)))

;;; Statistics

(defun count-tasks-by-status (status)
  "Count tasks with given status."
  (let* ((query (dbi:prepare *db*
                             "SELECT COUNT(*) as count FROM tasks WHERE status = ? AND deleted = 0"))
         (result (dbi:execute query (list (task-status-string status))))
         (row (dbi:fetch result)))
    (or (getf row :|count|) 0)))

(defun count-tasks-due-today ()
  "Count tasks due today."
  (let* ((today (date-to-string (local-time:today)))
         (query (dbi:prepare *db*
                             "SELECT COUNT(*) as count FROM tasks WHERE due_date = ? AND status != 'completed' AND deleted = 0"))
         (result (dbi:execute query (list today)))
         (row (dbi:fetch result)))
    (or (getf row :|count|) 0)))

(defun count-overdue-tasks ()
  "Count overdue tasks."
  (let* ((today (date-to-string (local-time:today)))
         (query (dbi:prepare *db*
                             "SELECT COUNT(*) as count FROM tasks WHERE due_date < ? AND status != 'completed' AND deleted = 0"))
         (result (dbi:execute query (list today)))
         (row (dbi:fetch result)))
    (or (getf row :|count|) 0)))

(defun count-tasks-for-project (project-id)
  "Count tasks in a project."
  (let* ((query (dbi:prepare *db*
                             "SELECT COUNT(*) as count FROM tasks WHERE project_id = ? AND deleted = 0"))
         (result (dbi:execute query (list project-id)))
         (row (dbi:fetch result)))
    (or (getf row :|count|) 0)))
