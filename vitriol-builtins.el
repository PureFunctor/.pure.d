(require 'ansi-color)
(require 'vitriol-constants)

(defun vitriol/builtins/modes ()
  "Load global modes."
  (menu-bar-mode   -1)
  (tool-bar-mode   -1)
  (scroll-bar-mode -1)
  (tooltip-mode    -1)
  (fringe-mode      0)
  (column-number-mode)
  (electric-pair-mode)
  (global-auto-revert-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'compilation-filter-hook 'vitriol/builtins/colorize-compilation-buffer))

(defun vitriol/builtins/binds ()
  "Load global binds."
  (setq-default indent-tabs-mode nil)
  (setq-default cursor-type 'box)
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

(defun vitriol/builtins/fonts ()
  "Load font configuration."
  (set-face-attribute 'default nil :font "Mononoki" :height 108)
  (set-face-attribute 'fixed-pitch nil :font "Mononoki" :height 108)
  (set-face-attribute 'variable-pitch nil :font "Cormorant Garamond" :height 108))

(defun vitriol/builtins/frame ()
  "Load frame configuration."
  (set-frame-parameter (selected-frame) 'alpha '(95 . 90))
  (add-to-list 'default-frame-alist '(alpha . (95 . 90))))

(defun vitriol/builtins/advice ()
  (defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer)))

(defun vitriol/builtins/entry ()
  "Entry point for `vitriol-builtins.el'."
  (vitriol/builtins/modes)
  (vitriol/builtins/binds)
  (vitriol/builtins/fonts)
  (vitriol/builtins/frame)
  (vitriol/builtins/advice))

(defun vitriol/builtins/colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(provide 'vitriol-builtins)
