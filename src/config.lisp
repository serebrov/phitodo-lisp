;;;; config.lisp - Configuration management

(in-package #:phitodo-tui.config)

(defclass config ()
  ((db-path
    :initarg :db-path
    :accessor config-db-path
    :initform nil
    :documentation "Path to SQLite database file")
   (github-token
    :initarg :github-token
    :accessor config-github-token
    :initform nil
    :documentation "GitHub API token")
   (github-username
    :initarg :github-username
    :accessor config-github-username
    :initform nil
    :documentation "GitHub username")
   (toggl-token
    :initarg :toggl-token
    :accessor config-toggl-token
    :initform nil
    :documentation "Toggl API token")
   (toggl-workspace-id
    :initarg :toggl-workspace-id
    :accessor config-toggl-workspace-id
    :initform nil
    :documentation "Toggl workspace ID"))
  (:documentation "Application configuration"))

(defvar *config* nil
  "Global configuration instance")

(defun config-dir ()
  "Return the configuration directory path."
  (merge-pathnames #P".config/phitodo-tui/"
                   (user-homedir-pathname)))

(defun data-dir ()
  "Return the data directory path."
  (merge-pathnames #P".local/share/phitodo-tui/"
                   (user-homedir-pathname)))

(defun config-file-path ()
  "Return the path to the config file."
  (merge-pathnames "config.lisp" (config-dir)))

(defun default-db-path ()
  "Return the default database path."
  (merge-pathnames "phitodo.db" (data-dir)))

(defun ensure-dirs ()
  "Ensure configuration and data directories exist."
  (ensure-directories-exist (config-dir))
  (ensure-directories-exist (data-dir)))

(defun load-config (&optional custom-path)
  "Load configuration from file or create default.
If CUSTOM-PATH is provided, load from that file instead."
  (ensure-dirs)
  (let ((config-path (or custom-path (config-file-path))))
    (if (probe-file config-path)
        (with-open-file (stream config-path :direction :input)
          (let ((data (read stream nil nil)))
            (setf *config*
                  (make-instance 'config
                                 :db-path (or (getf data :db-path)
                                              (namestring (default-db-path)))
                                 :github-token (getf data :github-token)
                                 :github-username (getf data :github-username)
                                 :toggl-token (getf data :toggl-token)
                                 :toggl-workspace-id (getf data :toggl-workspace-id)))))
        (setf *config*
              (make-instance 'config
                             :db-path (namestring (default-db-path)))))
    *config*))

(defun save-config ()
  "Save current configuration to file."
  (ensure-dirs)
  (when *config*
    (with-open-file (stream (config-file-path)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*print-pretty* t))
        (print `(:db-path ,(config-db-path *config*)
                 :github-token ,(config-github-token *config*)
                 :github-username ,(config-github-username *config*)
                 :toggl-token ,(config-toggl-token *config*)
                 :toggl-workspace-id ,(config-toggl-workspace-id *config*))
               stream)))))
