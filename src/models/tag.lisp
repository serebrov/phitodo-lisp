;;;; tag.lisp - Tag model

(in-package #:phitodo-tui.models)

(defclass tag ()
  ((id
    :initarg :id
    :accessor tag-id
    :initform (format nil "~A" (uuid:make-v4-uuid))
    :documentation "Unique identifier")
   (name
    :initarg :name
    :accessor tag-name
    :initform ""
    :documentation "Tag name")
   (color
    :initarg :color
    :accessor tag-color
    :initform nil
    :documentation "Display color")
   (created-at
    :initarg :created-at
    :accessor tag-created-at
    :initform (local-time:now)
    :documentation "Creation timestamp")
   (updated-at
    :initarg :updated-at
    :accessor tag-updated-at
    :initform (local-time:now)
    :documentation "Last update timestamp")
   (deleted-p
    :initarg :deleted-p
    :accessor tag-deleted-p
    :initform nil
    :documentation "Soft delete flag"))
  (:documentation "A tag for categorizing tasks"))

(defun make-tag (name &rest args)
  "Create a new tag with the given name."
  (apply #'make-instance 'tag :name name args))

(defmethod print-object ((tag tag) stream)
  "Print tag representation."
  (print-unreadable-object (tag stream :type t)
    (format stream "~A #~A" (tag-id tag) (tag-name tag))))
