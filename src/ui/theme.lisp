;;;; theme.lisp - UI theme and colors

(in-package #:phitodo-tui.ui)

;;; Color definitions (using croatoan color pairs)
(defparameter *color-bg* :white)
(defparameter *color-fg* :black)
(defparameter *color-accent* :blue)
(defparameter *color-muted* :black)
(defparameter *color-success* :green)
(defparameter *color-warning* :yellow)
(defparameter *color-error* :red)
(defparameter *color-highlight* :blue)

;;; Sidebar item definitions
(deftype sidebar-item ()
  '(member :inbox :today :upcoming :anytime :completed :review :github :toggl :settings))

(defun sidebar-item-icon (item)
  "Get icon for sidebar item."
  (case item
    (:inbox "[I]")
    (:today "[T]")
    (:upcoming "[U]")
    (:anytime "[A]")
    (:completed "[x]")
    (:review "[R]")
    (:github "[G]")
    (:toggl "[O]")
    (:settings "[S]")
    (otherwise "[ ]")))

(defun sidebar-item-label (item)
  "Get label for sidebar item."
  (case item
    (:inbox "Inbox")
    (:today "Today")
    (:upcoming "Upcoming")
    (:anytime "Anytime")
    (:completed "Completed")
    (:review "Review")
    (:github "GitHub")
    (:toggl "Toggl")
    (:settings "Settings")
    (otherwise "Unknown")))

(defun sidebar-item-shortcut (item)
  "Get keyboard shortcut for sidebar item."
  (case item
    (:inbox "1")
    (:today "2")
    (:upcoming "3")
    (:anytime "4")
    (:completed "5")
    (:review "6")
    (:github "7")
    (:toggl "8")
    (:settings "9")
    (otherwise nil)))

(defparameter *sidebar-items*
  '(:inbox :today :upcoming :anytime :completed :review :github :toggl :settings)
  "All sidebar navigation items.")

(defparameter *main-views*
  '(:inbox :today :upcoming :anytime :completed :review)
  "Main task views.")

;;; Task display helpers
(defun priority-color (priority)
  "Get color for task priority."
  (case priority
    (:high *color-error*)
    (:medium *color-warning*)
    (:low *color-muted*)
    (otherwise *color-fg*)))

(defun status-symbol (status)
  "Get symbol for task status."
  (case status
    (:inbox "[I]")
    (:active "[>]")
    (:scheduled "[S]")
    (:completed "[x]")
    (:cancelled "[-]")
    (otherwise "[ ]")))

(defun priority-symbol (priority)
  "Get symbol for task priority."
  (case priority
    (:high "!!!")
    (:medium "!!")
    (:low "!")
    (otherwise "")))

(defun kind-symbol (kind)
  "Get symbol for task kind."
  (case kind
    (:task "[T]")
    (:bug "[B]")
    (:feature "[F]")
    (:chore "[C]")
    (:gh-issue "[ISS]")
    (:gh-pr "[PR]")
    (:gh-review "[REV]")
    (otherwise "")))

;;; Key binding display
(defparameter *help-bindings*
  '(("Navigation" . (("j/k" . "Up/Down")
                     ("h/l" . "Left/Right")
                     ("g/G" . "First/Last")
                     ("Tab" . "Next pane")
                     ("1-9" . "Switch view")))
    ("Tasks" . (("Enter" . "Edit task")
                ("Space" . "Toggle complete")
                ("n" . "New task")
                ("d" . "Delete task")
                ("o" . "Open URL")))
    ("Editing" . (("^A/^E" . "Home/End")
                  ("^K" . "Kill to end")
                  ("^U" . "Kill to start")
                  ("^W" . "Kill word")
                  ("^Y" . "Paste/Yank")))
    ("Other" . (("r" . "Refresh")
                ("?" . "Help")
                ("q" . "Quit"))))
  "Help text key bindings.")
