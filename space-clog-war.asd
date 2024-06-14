;;;; clog-fractalizer.asd

(asdf:defsystem #:space-clog-war
  :description "space-clog-wars: Spacewar! implemented with CLOG"
  :author "José M. Á. Ronquillo Rivera <jose@rufina.link>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:clog
               #:rtg-math
               #:mathkit)
  :components ((:file "package")
               (:file "clog-webgl-patch")
               (:file "display")
               (:file "spacewar")
               (:file "space-clog-war")))
