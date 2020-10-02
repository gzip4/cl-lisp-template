# cl-lisp-template
Simple templating engine allowing evaluation of Common-Lisp forms (for those who do not afraid parenthesis)

## Examples
```
<!doctype html>
<html>
<head>
  <!-- html-escaped value -->
  <title><%= (getf params :title "Default title") %></title>
  ...
</head>
...
<body>
<ul>
  <% (loop for x in (getf params :menus) %>
  <li><% do (princ x) %></li>
  <% ) %>
</ul>
<% (inc :other-template) %> <!-- include other template output -->
...
```

```
;; LISP
(ltp:deftemplate :main #p"template-path") ;; maybe string or stream

(defun main ()
  (let ((content (ltp:invoke-template :main (list :title "MAIN"))))
     ...

```
