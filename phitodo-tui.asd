;;;; phitodo-tui.asd - System definition for phitodo TUI

(asdf:defsystem #:phitodo-tui
  :description "A terminal-based task manager in Common Lisp"
  :author "phitodo"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria
               #:cl-dbi
               #:dbd-sqlite3
               #:local-time
               #:uuid
               #:yason
               #:dexador
               #:croatoan
               #:bordeaux-threads
               #:str
               #:uiop)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "config")
                 (:module "models"
                  :components ((:file "task")
                               (:file "project")
                               (:file "tag")))
                 (:module "db"
                  :components ((:file "schema")
                               (:file "repository")))
                 (:module "services"
                  :components ((:file "filter")
                               (:file "github")))
                 (:module "ui"
                  :components ((:file "theme")
                               (:file "components")
                               (:file "views")
                               (:file "app")))
                 (:file "main")))))
