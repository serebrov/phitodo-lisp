;;;; task.lisp - Task model

(in-package #:phitodo-tui.models)

;;; Task Status
(deftype task-status ()
  '(member :inbox :active :scheduled :completed :cancelled))

(defconstant +task-status-inbox+ :inbox)
(defconstant +task-status-active+ :active)
(defconstant +task-status-scheduled+ :scheduled)
(defconstant +task-status-completed+ :completed)
(defconstant +task-status-cancelled+ :cancelled)

(defun task-status-string (status)
  "Convert task status to string for storage."
  (string-downcase (symbol-name status)))

(defun string-task-status (str)
  "Convert string to task status."
  (when str
    (intern (string-upcase str) :keyword)))

;;; Task Priority
(deftype task-priority ()
  '(member :none :low :medium :high))

(defconstant +priority-none+ :none)
(defconstant +priority-low+ :low)
(defconstant +priority-medium+ :medium)
(defconstant +priority-high+ :high)

(defun priority-string (priority)
  "Convert priority to string."
  (string-downcase (symbol-name priority)))

(defun string-priority (str)
  "Convert string to priority."
  (if (or (null str) (string= str ""))
      :none
      (intern (string-upcase str) :keyword)))

(defun priority-symbol (priority)
  "Get display symbol for priority."
  (case priority
    (:high "!!!")
    (:medium "!!")
    (:low "!")
    (otherwise "")))

;;; Task Kind
(deftype task-kind ()
  '(member :task :bug :feature :chore :gh-issue :gh-pr :gh-review))

(defconstant +kind-task+ :task)
(defconstant +kind-bug+ :bug)
(defconstant +kind-feature+ :feature)
(defconstant +kind-chore+ :chore)
(defconstant +kind-gh-issue+ :gh-issue)
(defconstant +kind-gh-pr+ :gh-pr)
(defconstant +kind-gh-review+ :gh-review)

(defun kind-string (kind)
  "Convert kind to string for storage (matching Rust format)."
  (when kind
    (case kind
      (:gh-issue "gh:issue")
      (:gh-pr "gh:pr")
      (:gh-review "gh:review")
      (otherwise (string-downcase (symbol-name kind))))))

(defun string-kind (str)
  "Convert string to kind."
  (when (and str (not (string= str "")))
    (cond
      ((string= str "gh:issue") :gh-issue)
      ((string= str "gh:pr") :gh-pr)
      ((string= str "gh:review") :gh-review)
      ;; Also handle old format with dashes
      ((string= str "gh-issue") :gh-issue)
      ((string= str "gh-pr") :gh-pr)
      ((string= str "gh-review") :gh-review)
      (t (intern (string-upcase str) :keyword)))))

(defun kind-symbol (kind)
  "Get display symbol for kind (matching Rust format)."
  (case kind
    (:task "[T]")
    (:bug "[B]")
    (:feature "[F]")
    (:chore "[C]")
    (:gh-issue "[ISS]")
    (:gh-pr "[PR]")
    (:gh-review "[REV]")
    (otherwise "")))

;;; Task Size
(deftype task-size ()
  '(member :xs :s :m :l))

(defconstant +size-xs+ :xs)
(defconstant +size-s+ :s)
(defconstant +size-m+ :m)
(defconstant +size-l+ :l)

(defun size-string (size)
  "Convert size to string."
  (when size
    (string-downcase (symbol-name size))))

(defun string-size (str)
  "Convert string to size."
  (when (and str (not (string= str "")))
    (intern (string-upcase str) :keyword)))

(defun size-display (size)
  "Get display string for size."
  (case size
    (:xs "XS")
    (:s "S")
    (:m "M")
    (:l "L")
    (otherwise "")))

;;; Task Class
(defclass task ()
  ((id
    :initarg :id
    :accessor task-id
    :initform (format nil "~A" (uuid:make-v4-uuid))
    :documentation "Unique identifier")
   (title
    :initarg :title
    :accessor task-title
    :initform ""
    :documentation "Task title")
   (notes
    :initarg :notes
    :accessor task-notes
    :initform nil
    :documentation "Additional notes/description")
   (created-at
    :initarg :created-at
    :accessor task-created-at
    :initform (local-time:now)
    :documentation "Creation timestamp")
   (updated-at
    :initarg :updated-at
    :accessor task-updated-at
    :initform (local-time:now)
    :documentation "Last update timestamp")
   (due-date
    :initarg :due-date
    :accessor task-due-date
    :initform nil
    :documentation "Due date (local-time timestamp)")
   (start-date
    :initarg :start-date
    :accessor task-start-date
    :initform nil
    :documentation "Start date for scheduling")
   (completed-at
    :initarg :completed-at
    :accessor task-completed-at
    :initform nil
    :documentation "Completion timestamp")
   (project-id
    :initarg :project-id
    :accessor task-project-id
    :initform nil
    :documentation "Associated project ID")
   (priority
    :initarg :priority
    :accessor task-priority
    :initform :none
    :type task-priority
    :documentation "Task priority level")
   (tags
    :initarg :tags
    :accessor task-tags
    :initform nil
    :documentation "List of tag IDs")
   (status
    :initarg :status
    :accessor task-status
    :initform :inbox
    :type task-status
    :documentation "Current status")
   (order-index
    :initarg :order-index
    :accessor task-order-index
    :initform 0
    :documentation "Ordering within views")
   (deleted-p
    :initarg :deleted-p
    :accessor task-deleted-p
    :initform nil
    :documentation "Soft delete flag")
   (kind
    :initarg :kind
    :accessor task-kind
    :initform nil
    :documentation "Type of task")
   (size
    :initarg :size
    :accessor task-size
    :initform nil
    :documentation "Size estimate")
   (assignee
    :initarg :assignee
    :accessor task-assignee
    :initform nil
    :documentation "Assigned user")
   (context-url
    :initarg :context-url
    :accessor task-context-url
    :initform nil
    :documentation "External URL (e.g., GitHub issue)")
   (metadata
    :initarg :metadata
    :accessor task-metadata
    :initform (make-hash-table :test 'equal)
    :documentation "Additional metadata"))
  (:documentation "A task item"))

(defun make-task (title &rest args)
  "Create a new task with the given title."
  (apply #'make-instance 'task :title title args))

(defmethod task-completed-p ((task task))
  "Check if task is completed."
  (eq (task-status task) :completed))

(defmethod task-overdue-p ((task task))
  "Check if task is overdue."
  (and (task-due-date task)
       (not (task-completed-p task))
       (local-time:timestamp< (task-due-date task)
                               (local-time:today))))

(defmethod task-due-today-p ((task task))
  "Check if task is due today."
  (and (task-due-date task)
       (not (task-completed-p task))
       (local-time:timestamp=
        (local-time:timestamp-minimize-part (task-due-date task) :hour)
        (local-time:today))))

(defmethod complete-task ((task task))
  "Mark task as completed."
  (setf (task-status task) :completed)
  (setf (task-completed-at task) (local-time:now))
  task)

(defmethod print-object ((task task) stream)
  "Print task representation."
  (print-unreadable-object (task stream :type t)
    (format stream "~A ~S" (task-id task) (task-title task))))
