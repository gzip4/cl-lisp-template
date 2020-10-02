;;;; qtemplate.lisp

(in-package :cl-lisp-template)

(defvar *templates* (make-hash-table))


(defun %split-by (p src)
  (let* ((found)
	 (r (with-output-to-string (out)
	      (loop
		 :with i = 0
		 :with plen = (length p)
		 :for c = (read-char src nil)
		 :do (unless c (return))
		 :do (if (char= (elt p i) c)
			 (incf i)
			 (progn
			   (when (> i 0)
			     (write-sequence p out :end i)
			     (setf i 0))
			   (write-char c out)))
		 :do (when (>= i plen)
		       (setf found t)
		       (return))))))
    (values found r)))


(defun %destructure (src)
  (let (r typ)
    (loop
       (multiple-value-bind (f1 p1) (%split-by "<%" src)
	 (setf typ :code)
	 (if f1 (let ((c (peek-char nil src)))
		  (when (and c (char= c #\=))
		    (read-char src nil)
		    (setf typ :val))))
	 (if f1
	     (multiple-value-bind (f2 p2) (%split-by "%>" src)
	       (if f2
		   (progn (push p1 r) (push (cons typ p2) r))
		   (error "Unbalansed delimiters")))
	     (progn
	       (push p1 r)
	       (return)))))
    (mapcar (lambda (x) (if (typep x 'string) (cons :str x) x))
	    (nreverse (delete "" r :test 'equalp)))))


(defun escape-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML
output. Hunchentoot copy-paste."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))


(defun %parse-template (src)
  (let ((q (%destructure src)))
    (with-output-to-string (s)
      (format s "(lambda (&optional params) (declare (ignorable params))")
      (loop
	 :with fs1 = "(write-string (escape-html (format nil ~s (progn ~a))))"
	 :for (x . y) :in q
	 :do (case x
	       (:str (format s "(write-string ~s)" y))
	       (:val (format s fs1 "~a" y))
	       (:code (write-string y s))))
      (format s "(values))"))))


(defun parse-template (src)
  (etypecase src
    (stream (%parse-template src))
    (string (with-input-from-string (s src) (%parse-template s)))
    (pathname (with-open-file (s src :external-format :utf-8)
		(%parse-template s)))))


(defun compile-template (src)
  (let ((*package* (find-package :cl-lisp-template/ns)))
    (compile nil (read-from-string (parse-template src)))))


(defun invoke-template (template &optional params)
  (with-output-to-string (s)
    (let ((*standard-output* s))
      (etypecase template
	(function (funcall template params))
	(symbol (funcall (gethash template *templates* #'identity) params))))))
      

(defmacro deftemplate (name src)
  (check-type name symbol)
  (let ((!src (gensym)) (!f (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,!src ,src))
	 (etypecase ,!src
	   (string
	    (setf (gethash ,name *templates*) (compile-template ,!src)))
	   (pathname
	    (setf (gethash ,name *templates*)
		  (with-open-file (,!f ,!src :external-format :utf-8)
		    (compile-template ,!f)))))))))


(defmacro inc (name &optional params2)
  (check-type name symbol)
  (if params2
      `(funcall (gethash ,name *templates* #'identity) ,params2)
      `(funcall (gethash ,name *templates* #'identity) cl-lisp-template/ns::params)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(inc) :cl-lisp-template/ns))

