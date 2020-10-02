;;;; cl-lisp-template.asd

(in-package :cl-user)

(asdf:defsystem #:cl-lisp-template
  :serial t
  :version "1.0"
  :description "Simple templating engine allowing evaluation of Common-Lisp forms"
  :author "Kirill Zverev <gzip4ever@gmail.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "qtemplate")))

