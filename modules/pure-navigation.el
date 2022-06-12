;;; pure-navigation.el --- Configuration related to Emacs navigation

;;; Commentary:
;;
;; This module contains configuration regarding navigation.
(require 'pure-setup)

;;; Code:

;;;; Ace Window
(straight-use-package 'ace-window)

(setup (:require ace-window)
  (:option aw-dispatch-always t
           aw-display-mode-overlay t
           aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
  (:global "M-o" ace-window)
  (ace-window-display-mode))

;;;; Avy
(straight-use-package 'avy)

(setup (:require avy)
  (:global "C-;" avy-goto-char-timer))

;;; Ibuffer
(setup (:require ibuffer)
  (:global "C-x C-b" ibuffer))

(provide 'pure-navigation)

;;; Ctrlf
(straight-use-package 'ctrlf)

(setup (:require ctrlf)
  (ctrlf-mode))

;;; pure-navigation.el ends here
