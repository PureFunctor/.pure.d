;;; pure-org.el --- Configuration related to Org Mode

;;; Commentary:
;;
;; This module contains configuration for Org Mode.

;;; Code:

;;;; Org
(straight-use-package 'org)

(setup (:require org)
  (:option org-directory "~/PureFunctor/Org/Agenda"
           org-agenda-files (list org-directory)
           org-hide-emphasis-markers t
           org-edit-src-content-indentation 0)
  (:hook org-indent-mode))

;;;; Org Superstar
(straight-use-package 'org-superstar)

(setup (:require org-superstar)
  (:option org-superstar-leading-bullet " "
           org-superstar-headline-bullets-list '(?▶ ?▷))
  (:hook-into org-mode-hook))

(provide 'pure-org)

;;; pure-org.el ends here
