;;;; packages.lisp - Package definitions for phitodo-tui

(defpackage #:phitodo-tui.models
  (:use #:cl #:alexandria)
  (:export
   ;; Task status
   #:task-status
   #:+task-status-inbox+
   #:+task-status-active+
   #:+task-status-scheduled+
   #:+task-status-completed+
   #:+task-status-cancelled+
   #:task-status-string
   #:string-task-status
   ;; Task priority
   #:task-priority
   #:+priority-none+
   #:+priority-low+
   #:+priority-medium+
   #:+priority-high+
   #:priority-string
   #:string-priority
   #:priority-symbol
   ;; Task kind
   #:task-kind
   #:+kind-task+
   #:+kind-bug+
   #:+kind-feature+
   #:+kind-chore+
   #:+kind-gh-issue+
   #:+kind-gh-pr+
   #:+kind-gh-review+
   #:kind-string
   #:string-kind
   #:kind-symbol
   ;; Task size
   #:task-size
   #:+size-xs+
   #:+size-s+
   #:+size-m+
   #:+size-l+
   #:size-string
   #:string-size
   #:size-display
   ;; Task class
   #:task
   #:make-task
   #:task-id
   #:task-title
   #:task-notes
   #:task-created-at
   #:task-updated-at
   #:task-due-date
   #:task-start-date
   #:task-completed-at
   #:task-project-id
   #:task-priority
   #:task-tags
   #:task-status
   #:task-order-index
   #:task-deleted-p
   #:task-kind
   #:task-size
   #:task-assignee
   #:task-context-url
   #:task-metadata
   #:task-completed-p
   #:task-overdue-p
   #:task-due-today-p
   #:complete-task
   ;; Project class
   #:project
   #:make-project
   #:project-id
   #:project-name
   #:project-description
   #:project-color
   #:project-icon
   #:project-order-index
   #:project-inbox-p
   #:project-created-at
   #:project-updated-at
   #:project-deleted-p
   #:project-display-icon
   ;; Tag class
   #:tag
   #:make-tag
   #:tag-id
   #:tag-name
   #:tag-color
   #:tag-created-at
   #:tag-updated-at
   #:tag-deleted-p))

(defpackage #:phitodo-tui.config
  (:use #:cl #:alexandria)
  (:export
   #:*config*
   #:config
   #:config-db-path
   #:config-github-token
   #:config-github-username
   #:config-toggl-token
   #:config-toggl-workspace-id
   #:load-config
   #:save-config
   #:ensure-dirs
   #:default-db-path))

(defpackage #:phitodo-tui.db
  (:use #:cl #:alexandria #:phitodo-tui.models)
  (:export
   #:*db*
   #:connect-db
   #:disconnect-db
   #:with-db
   #:init-schema
   ;; Task operations
   #:get-all-tasks
   #:get-task
   #:insert-task
   #:update-task
   #:delete-task
   ;; Project operations
   #:get-all-projects
   #:get-project
   #:insert-project
   #:update-project
   #:delete-project
   ;; Tag operations
   #:get-all-tags
   #:get-tag
   #:insert-tag
   #:update-tag
   #:delete-tag
   ;; Stats
   #:count-tasks-by-status
   #:count-tasks-due-today
   #:count-overdue-tasks
   #:count-tasks-for-project))

(defpackage #:phitodo-tui.services
  (:use #:cl #:alexandria #:phitodo-tui.models)
  (:export
   #:filter-inbox
   #:filter-today
   #:filter-upcoming
   #:filter-anytime
   #:filter-completed
   #:filter-review
   #:filter-by-project
   #:filter-by-tag
   #:search-tasks
   #:sort-by-due-date
   #:sort-by-priority
   ;; GitHub
   #:sync-github-issues
   #:sync-github-prs
   #:sync-github-reviews
   #:sync-all-github
   #:sync-github-to-tasks))

(defpackage #:phitodo-tui.ui
  (:use #:cl #:alexandria #:croatoan
        #:phitodo-tui.models
        #:phitodo-tui.config
        #:phitodo-tui.db
        #:phitodo-tui.services)
  (:export
   #:run-app
   #:start))

(defpackage #:phitodo-tui
  (:use #:cl)
  (:export #:main
           #:run))
