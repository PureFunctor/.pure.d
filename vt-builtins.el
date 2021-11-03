(require 'ansi-color)
(require 'vt-constants)

(defun vt/builtins/modes ()
  "Load global modes."
  (column-number-mode)
  (electric-pair-mode)
  (global-auto-revert-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'compilation-filter-hook 'vt/builtins/colorize-compilation-buffer))

(defun vt/builtins/binds ()
  "Load global binds."
  (setq-default indent-tabs-mode nil
                cursor-type 'box)
  (setq
    inhibit-startup-message t
    require-final-newline t
    custom-file user-custom-file
    read-process-output-max (* 1024 32)
    dired-listing-switches "-alvF --group-directories-first"
    backup-directory-alist `((".*" . ,user-backup-directory))
    auto-save-file-name-transforms `((".*" ,user-backup-directory t))
    shift-select-mode nil
    help-window-select t))

(defun vt/builtins/fonts ()
  "Load font configuration."
  (set-face-attribute 'default nil :font "Mononoki" :height 108)
  (set-face-attribute 'fixed-pitch nil :font "Mononoki" :height 108)
  (set-face-attribute 'variable-pitch nil :font "Cormorant Garamond" :height 108))

(defun vt/builtins/advice ()
  (defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer)))

(defun vt/builtins/entry ()
  "Entry point for `vt-builtins.el'."
  (vt/builtins/modes)
  (vt/builtins/binds)
  (vt/builtins/fonts)
  (vt/builtins/advice))

(defun vt/builtins/colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(provide 'vt-builtins)
