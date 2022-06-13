;;; pure-extra.el --- Extra tools and modes

;;; Commentary:
;;
;; This module contains extra tools and modes
(require 'pure-setup)

;;; Code:

;;;; Mastodon
(straight-use-package 'mastodon)

(setup (:require mastodon)
  (:option mastodon-active-user "purefunctor"
           mastodon-instance-url "https://types.pl"))

;;;; Perspective.el
(straight-use-package 'perspective)

(setup (:require perspective)
  (:option persp-mode-prefix-key (kbd "M-p")
           persp-state-default-file "~/.emacs.d/perspective")

  (:when-loaded (persp-mode))

  (defun my/persp-load-state ()
    "Load the state."
    (interactive)
    (persp-state-load persp-state-default-file)
    (persp-kill "main"))

  (:bind-into perspective-map "C-l" my/persp-load-state))

(provide 'pure-extra)
;;; pure-extra.el ends here
