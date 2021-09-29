# .pure.d
Emacs configuration.

# Installation
Clone this repository to `"~/.pure.d/"` and add the following to `init.el`
```lisp
(add-to-list 'load-path "~/.pure.d/")
(add-to-list 'load-path "~/.pure.d/lisp")
(require 'config)
(config/entry)
```
