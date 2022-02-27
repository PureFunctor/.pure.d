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
  (:global "C-c w" ace-window)
  (ace-window-display-mode))

;;;; Avy
(straight-use-package 'avy)

(setup (:require avy)
  (:global "C-c f w" avy-goto-word-1
	   "C-c f t" avy-goto-line))

;;; Ibuffer
(setup (:require ibuffer)
  (:global "C-x C-b" ibuffer))

(provide 'pure-navigation)

;;; pure-navigation.el ends here
