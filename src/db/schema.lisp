;;;; schema.lisp - Database schema definitions

(in-package #:phitodo-tui.db)

(defparameter *schema-version* 1)

(defparameter *create-projects-table*
  "CREATE TABLE IF NOT EXISTS projects (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT,
    color TEXT,
    icon TEXT,
    order_index INTEGER NOT NULL DEFAULT 0,
    is_inbox INTEGER NOT NULL DEFAULT 0,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    deleted INTEGER NOT NULL DEFAULT 0
  )")

(defparameter *create-tags-table*
  "CREATE TABLE IF NOT EXISTS tags (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    color TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    deleted INTEGER NOT NULL DEFAULT 0
  )")

(defparameter *create-tasks-table*
  "CREATE TABLE IF NOT EXISTS tasks (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    notes TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    due_date TEXT,
    start_date TEXT,
    completed_at TEXT,
    project_id TEXT REFERENCES projects(id),
    priority TEXT NOT NULL DEFAULT 'none',
    status TEXT NOT NULL DEFAULT 'inbox',
    order_index INTEGER NOT NULL DEFAULT 0,
    deleted INTEGER NOT NULL DEFAULT 0,
    kind TEXT,
    size TEXT,
    assignee TEXT,
    context_url TEXT,
    metadata TEXT
  )")

(defparameter *create-task-tags-table*
  "CREATE TABLE IF NOT EXISTS task_tags (
    task_id TEXT NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,
    tag_id TEXT NOT NULL REFERENCES tags(id) ON DELETE CASCADE,
    PRIMARY KEY (task_id, tag_id)
  )")

(defparameter *create-schema-version-table*
  "CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY
  )")

(defparameter *create-indexes*
  '("CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status)"
    "CREATE INDEX IF NOT EXISTS idx_tasks_due_date ON tasks(due_date)"
    "CREATE INDEX IF NOT EXISTS idx_tasks_project ON tasks(project_id)"
    "CREATE INDEX IF NOT EXISTS idx_task_tags_task ON task_tags(task_id)"
    "CREATE INDEX IF NOT EXISTS idx_task_tags_tag ON task_tags(tag_id)"))

(defun init-schema (db)
  "Initialize database schema."
  ;; Create tables
  (dbi:do-sql db *create-projects-table*)
  (dbi:do-sql db *create-tags-table*)
  (dbi:do-sql db *create-tasks-table*)
  (dbi:do-sql db *create-task-tags-table*)
  (dbi:do-sql db *create-schema-version-table*)
  ;; Create indexes
  (dolist (index-sql *create-indexes*)
    (dbi:do-sql db index-sql))
  ;; Set schema version
  (dbi:do-sql db "INSERT OR REPLACE INTO schema_version (version) VALUES (?)"
              (list *schema-version*)))
