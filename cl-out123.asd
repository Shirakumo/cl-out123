(asdf:defsystem cl-out123
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Bindings to libout123, providing cross-platform audio output."
  :homepage "https://shirakumo.org/docs/cl-out123/"
  :bug-tracker "https://shirakumo.org/project/cl-out123/issues"
  :source-control (:git "https://shirakumo.org/project/cl-out123.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :documentation-utils
               :bordeaux-threads))
