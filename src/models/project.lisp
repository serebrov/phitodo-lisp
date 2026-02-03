;;;; project.lisp - Project model

(in-package #:phitodo-tui.models)

(defclass project ()
  ((id
    :initarg :id
    :accessor project-id
    :initform (format nil "~A" (uuid:make-v4-uuid))
    :documentation "Unique identifier")
   (name
    :initarg :name
    :accessor project-name
    :initform ""
    :documentation "Project name")
   (description
    :initarg :description
    :accessor project-description
    :initform nil
    :documentation "Project description")
   (color
    :initarg :color
    :accessor project-color
    :initform nil
    :documentation "Display color")
   (icon
    :initarg :icon
    :accessor project-icon
    :initform nil
    :documentation "Display icon")
   (order-index
    :initarg :order-index
    :accessor project-order-index
    :initform 0
    :documentation "Ordering position")
   (inbox-p
    :initarg :inbox-p
    :accessor project-inbox-p
    :initform nil
    :documentation "Is this the inbox project?")
   (created-at
    :initarg :created-at
    :accessor project-created-at
    :initform (local-time:now)
    :documentation "Creation timestamp")
   (updated-at
    :initarg :updated-at
    :accessor project-updated-at
    :initform (local-time:now)
    :documentation "Last update timestamp")
   (deleted-p
    :initarg :deleted-p
    :accessor project-deleted-p
    :initform nil
    :documentation "Soft delete flag"))
  (:documentation "A project for organizing tasks"))

(defun make-project (name &rest args)
  "Create a new project with the given name."
  (apply #'make-instance 'project :name name args))

(defmethod project-display-icon ((project project))
  "Get display icon for project."
  (or (project-icon project) "[D]"))

(defmethod print-object ((project project) stream)
  "Print project representation."
  (print-unreadable-object (project stream :type t)
    (format stream "~A ~S" (project-id project) (project-name project))))
