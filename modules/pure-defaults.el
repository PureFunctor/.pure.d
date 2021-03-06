;;; pure-defaults.el --- Configuration regarding Emacs' defaults

;;; Commentary:
;;
;; This module contains configuration regarding Emacs' defaults.

(set-face-attribute 'default nil :font "Fantasque Sans Mono" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Fantasque Sans Mono " :height 120)
(set-face-attribute 'variable-pitch nil :font "Fantasque Sans Mono" :height 120)

(column-number-mode)
(electric-pair-mode)
(global-auto-revert-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq custom-file "~/.emacs.d/custom.el")

(provide 'pure-defaults)

;;; pure-defaults.el ends here
