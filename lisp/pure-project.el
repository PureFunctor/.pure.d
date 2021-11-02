;;; pure-project.el - Extras for Project
;;;
;;; -*- lexical-binding: t -*-
(defun pure/project-ansi-term ()
  "Start `ansi-term' in the current project's root directory."
  (interactive)
  (let* ((default-directory (project-root (project-current t))))
    (ansi-term "/usr/bin/zsh")))

(provide 'pure-project)
