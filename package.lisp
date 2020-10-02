;;;; package.lisp

(in-package :cl-user)

(defpackage :cl-lisp-template
  (:nicknames :ltp)
  (:use :cl)
  (:export #:escape-html
	   #:compile-template
	   #:invoke-template
	   #:deftemplate))

(defpackage :cl-lisp-template/ns
  (:use :cl)
  (:import-from :cl-lisp-template :escape-html))

