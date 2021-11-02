# .pure.d
Vitriol Emacs Distribution.

# Installation
Clone this repository to `"~/.pure.d/"` and add the following to `init.el`
```lisp
(add-to-list 'load-path "~/.pure.d/")
(add-to-list 'load-path "~/.pure.d/lisp")
(load "vt-bootstrap.el")
(require 'vt-config)
(vt/config/entry)
```
