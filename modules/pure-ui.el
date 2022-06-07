;;; pure-ui.el --- UI-related configuration

;;; Commentary:
;;
;; This module contains UI-related configuration.
(require 'pure-setup)

;;; Code:

;;;; All The Icons
(straight-use-package 'all-the-icons)

;;;; Dashboard
(straight-use-package 'dashboard)

(setup (:require dashboard)
  (:bind "n" dashboard-next-line
         "p" dashboard-previous-line
         "f" dashboard-next-section
         "b" dashboard-previous-section)
  (:option dashboard-center-content t
           dashboard-banner-logo-title "Emacs!!!"
           dashboard-startup-banner "~/.emacs.d/logo.png"
           dashboard-items '((recents . 5) (agenda . 5))
           dashboard-set-heading-icons t
           dashboard-set-file-icons t)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (agenda . "book")))
  (dashboard-setup-startup-hook))

;;;; Doom Themes
(straight-use-package 'doom-themes)

(setup (:require doom-themes)
  (setq doom-horizon-brighter-comments t)
  (load-theme 'doom-tomorrow-night t))

;;;; Mini Modeline
(straight-use-package 'mini-modeline)

(setup (:require mini-modeline)
  (mini-modeline-mode))

(provide 'pure-ui)

;;;; Diredfl
(straight-use-package 'diredfl)

(setup (:require diredfl)
  (:hook-into dired-mode-hook))

;;;; Highlight Indent Guides
(straight-use-package 'highlight-indent-guides)

(setup (:require highlight-indent-guides)
  (setq highlight-indent-guides-method 'character)
  (:hook-into prog-mode-hook))

;;;; Rainbow Delimiters
(straight-use-package 'rainbow-delimiters)

(setup (:require rainbow-delimiters)
  (:hook-into prog-mode-hook))

;;; pure-ui.el ends here
