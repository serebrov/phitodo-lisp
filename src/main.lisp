;;;; main.lisp - Application entry point

(in-package #:phitodo-tui)

(defun main (&rest args)
  "Main entry point for the application."
  (declare (ignore args))
  (format t "Starting phitodo-tui...~%")
  (phitodo-tui.ui:start))

(defun run ()
  "Convenience function to run the application."
  (main))

;;; For building a standalone executable
#+sbcl
(defun build-executable ()
  "Build a standalone executable."
  (sb-ext:save-lisp-and-die "phitodo-tui"
                            :toplevel #'main
                            :executable t
                            :compression t))
