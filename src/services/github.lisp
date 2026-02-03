;;;; github.lisp - GitHub API integration

(in-package #:phitodo-tui.services)

(defvar *github-base-url* "https://api.github.com")

(defun github-request (endpoint token)
  "Make a GET request to the GitHub API."
  (handler-case
      (multiple-value-bind (body status)
          (dex:get (concatenate 'string *github-base-url* endpoint)
                   :headers `(("Authorization" . ,(concatenate 'string "token " token))
                              ("Accept" . "application/vnd.github.v3+json")
                              ("User-Agent" . "phitodo-tui")))
        (if (= status 200)
            (values (yason:parse body) nil)
            (values nil (format nil "HTTP ~A" status))))
    (error (e)
      (values nil (format nil "Error: ~A" e)))))

(defun fetch-github-issues (token &optional (state "open"))
  "Fetch issues assigned to the authenticated user."
  (github-request (format nil "/issues?filter=assigned&state=~A&per_page=100" state) token))

(defun fetch-github-created-prs (token)
  "Fetch pull requests created by the authenticated user."
  (github-request "/search/issues?q=is:pr+is:open+author:@me&per_page=100" token))

(defun fetch-github-review-requests (token)
  "Fetch pull requests waiting for user's review."
  (github-request "/search/issues?q=is:pr+is:open+review-requested:@me&per_page=100" token))

(defun fetch-github-notifications (token)
  "Fetch unread notifications."
  (github-request "/notifications" token))

(defun github-item-to-task (item kind)
  "Convert a GitHub issue/PR to a task."
  (let* ((title (gethash "title" item))
         (number (gethash "number" item))
         (html-url (gethash "html_url" item))
         (body (gethash "body" item))
         (repo (gethash "repository" item))
         (repo-name (when repo (gethash "full_name" repo)))
         (task (phitodo-tui.models:make-task
                (format nil "#~A ~A" number title)
                :kind kind
                :context-url html-url
                :notes (when (and body (not (string= body ""))) body)
                :status :active)))
    ;; Store repo info in metadata
    (when repo-name
      (setf (gethash "github_repo" (phitodo-tui.models:task-metadata task)) repo-name))
    (setf (gethash "github_number" (phitodo-tui.models:task-metadata task)) number)
    task))

(defun extract-search-items (response)
  "Extract items from a GitHub search response."
  (if (hash-table-p response)
      (gethash "items" response)
      response))

(defun sync-github-issues (token)
  "Sync GitHub issues and return list of tasks."
  (multiple-value-bind (issues error) (fetch-github-issues token)
    (if error
        (values nil error)
        (values (mapcar (lambda (item)
                          (github-item-to-task item :gh-issue))
                        issues)
                nil))))

(defun sync-github-prs (token)
  "Sync GitHub PRs created by user and return list of tasks."
  (multiple-value-bind (response error) (fetch-github-created-prs token)
    (if error
        (values nil error)
        (let ((items (extract-search-items response)))
          (values (mapcar (lambda (item)
                            (github-item-to-task item :gh-pr))
                          items)
                  nil)))))

(defun sync-github-reviews (token)
  "Sync GitHub PRs waiting for review and return list of tasks."
  (multiple-value-bind (response error) (fetch-github-review-requests token)
    (if error
        (values nil error)
        (let ((items (extract-search-items response)))
          (values (mapcar (lambda (item)
                            (github-item-to-task item :gh-review))
                          items)
                  nil)))))

(defun sync-all-github (token)
  "Sync all GitHub data: issues, PRs, and review requests."
  (let ((all-issues nil)
        (all-prs nil)
        (all-reviews nil)
        (errors nil))
    ;; Fetch issues
    (multiple-value-bind (issues error) (sync-github-issues token)
      (if error
          (push (format nil "Issues: ~A" error) errors)
          (setf all-issues issues)))
    ;; Fetch PRs
    (multiple-value-bind (prs error) (sync-github-prs token)
      (if error
          (push (format nil "PRs: ~A" error) errors)
          (setf all-prs prs)))
    ;; Fetch reviews
    (multiple-value-bind (reviews error) (sync-github-reviews token)
      (if error
          (push (format nil "Reviews: ~A" error) errors)
          (setf all-reviews reviews)))
    ;; Return results
    (values all-issues all-prs all-reviews
            (when errors (format nil "~{~A~^, ~}" errors)))))

;;; Task synchronization to database

(defun find-task-by-url (url tasks)
  "Find a task by its context URL."
  (find url tasks
        :key #'phitodo-tui.models:task-context-url
        :test #'equal))

(defun find-or-create-project-for-repo (repo-name repo-to-project)
  "Find or create a project for a GitHub repository."
  (or (gethash repo-name repo-to-project)
      (let* ((existing-projects (phitodo-tui.db:get-all-projects))
             (existing (find repo-name existing-projects
                             :key #'phitodo-tui.models:project-name
                             :test #'equal)))
        (if existing
            (progn
              (setf (gethash repo-name repo-to-project)
                    (phitodo-tui.models:project-id existing))
              (phitodo-tui.models:project-id existing))
            ;; Create new project
            (let ((project (phitodo-tui.models:make-project repo-name
                            :icon "[GH]")))
              (phitodo-tui.db:insert-project project)
              (setf (gethash repo-name repo-to-project)
                    (phitodo-tui.models:project-id project))
              (phitodo-tui.models:project-id project))))))

(defun sync-github-item-to-db (item github-type existing-tasks repo-to-project seen-urls)
  "Sync a single GitHub item to the database.
Returns :created, :updated, or :unchanged."
  (let* ((url (phitodo-tui.models:task-context-url item))
         (existing (find-task-by-url url existing-tasks))
         (repo-name (gethash "github_repo" (phitodo-tui.models:task-metadata item)))
         (project-id (when repo-name
                       (find-or-create-project-for-repo repo-name repo-to-project))))
    ;; Track seen URLs
    (when url
      (setf (gethash url seen-urls) t))

    (if existing
        ;; Update existing task if needed
        (let ((changed nil))
          ;; Update project if not set
          (when (and project-id (null (phitodo-tui.models:task-project-id existing)))
            (setf (phitodo-tui.models:task-project-id existing) project-id)
            (setf changed t))
          ;; Update kind if not set
          (when (and (phitodo-tui.models:task-kind item)
                     (null (phitodo-tui.models:task-kind existing)))
            (setf (phitodo-tui.models:task-kind existing)
                  (phitodo-tui.models:task-kind item))
            (setf changed t))
          ;; Update notes if not set but item has notes
          (when (and (phitodo-tui.models:task-notes item)
                     (null (phitodo-tui.models:task-notes existing)))
            (setf (phitodo-tui.models:task-notes existing)
                  (phitodo-tui.models:task-notes item))
            (setf changed t))
          (when changed
            (phitodo-tui.db:update-task existing)
            :updated)
          :unchanged)
        ;; Create new task
        (progn
          (setf (phitodo-tui.models:task-project-id item) project-id)
          (setf (phitodo-tui.models:task-status item) :inbox)
          ;; Store github type in metadata
          (setf (gethash "github_type" (phitodo-tui.models:task-metadata item))
                github-type)
          (phitodo-tui.db:insert-task item)
          :created))))

(defun mark-closed-github-tasks (existing-tasks seen-urls)
  "Mark tasks as completed if their GitHub items are no longer open."
  (let ((completed-count 0))
    (dolist (task existing-tasks)
      (let ((url (phitodo-tui.models:task-context-url task)))
        ;; Only process GitHub tasks (those with context_url and github metadata)
        (when (and url
                   (gethash "github_type" (phitodo-tui.models:task-metadata task))
                   (not (gethash url seen-urls))
                   (not (phitodo-tui.models:task-completed-p task)))
          (phitodo-tui.models:complete-task task)
          (phitodo-tui.db:update-task task)
          (incf completed-count))))
    completed-count))

(defun sync-github-to-tasks (issues prs reviews)
  "Sync all GitHub items to the database as tasks.
Returns (values created-count updated-count completed-count)."
  (let ((existing-tasks (phitodo-tui.db:get-all-tasks))
        (repo-to-project (make-hash-table :test 'equal))
        (seen-urls (make-hash-table :test 'equal))
        (created 0)
        (updated 0))
    ;; Sync issues
    (dolist (item issues)
      (case (sync-github-item-to-db item "issue" existing-tasks repo-to-project seen-urls)
        (:created (incf created))
        (:updated (incf updated))))
    ;; Sync PRs
    (dolist (item prs)
      (case (sync-github-item-to-db item "my_pr" existing-tasks repo-to-project seen-urls)
        (:created (incf created))
        (:updated (incf updated))))
    ;; Sync reviews
    (dolist (item reviews)
      (case (sync-github-item-to-db item "review" existing-tasks repo-to-project seen-urls)
        (:created (incf created))
        (:updated (incf updated))))
    ;; Mark closed items as completed
    (let ((completed (mark-closed-github-tasks existing-tasks seen-urls)))
      (values created updated completed))))
