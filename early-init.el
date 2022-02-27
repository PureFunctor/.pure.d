(add-to-list 'load-path "~/.emacs.d/modules/")

(setq-default cursor-type 'box)
(setq-default indent-tabs-mode nil)

(setq gc-cons-threshold (* 2 1024 1024))
(setq read-process-output-max (* 1024 32))

(setq help-window-select t)
(setq inhibit-startup-message t)
(setq require-final-newline t)
(setq backup-directory-alist '((".*" . "saves")))
(setq auto-save-file-name-transforms '((".*" "saves" t)))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(alpha . (95 . 90)) default-frame-alist)
(push '(left-fringe . 8) default-frame-alist)
(push '(right-fringe . 8) default-frame-alist)
