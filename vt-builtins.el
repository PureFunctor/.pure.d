(require 'ansi-color)
(require 'vt-constants)

(column-number-mode)
(electric-pair-mode)
(global-auto-revert-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'compilation-filter-hook 'vt/builtins/colorize-compilation-buffer)

(setq-default
 indent-tabs-mode nil
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
 help-window-select t)

(set-face-attribute 'default nil :font "Mononoki" :height 105)
(set-face-attribute 'fixed-pitch nil :font "Mononoki" :height 105)
(set-face-attribute 'variable-pitch nil :font "Mononoki" :height 105)

(set-fontset-font "fontset-default"
                  'unicode
                  '("Fira Code" . "iso10646-1"))

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(defun vt/builtins/colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(provide 'vt-builtins)
