;;; pure-projects.el --- Configuration regarding the editing of source files

;;; Commentary:
;;
;; This module contains configuration regarding source editing
(require 'pure-setup)

;;; Code:

;;;; Project
(straight-use-package 'project)

(defun my/project-ansi-term ()
  "Start `ansi-term' in the current project's root directory."
  (interactive)
  (let* ((default-directory (project-root (project-current t))))
    (ansi-term shell-file-name)))

(setup (:require project)
  (:with-map project-prefix-map
    (:bind "t" my/project-ansi-term)))

;;;; Magit
(straight-use-package 'magit)

(setup (:require magit)
  (:option git-commit-summary-max-length 50
           transient-default-level 5))

;;;; Ws Butler
(straight-use-package 'ws-butler)

(setup (:require ws-butler)
  (:hook-into fundamental-mode prog-mode))

(provide 'pure-projects)

;;; pure-projects.el ends here
