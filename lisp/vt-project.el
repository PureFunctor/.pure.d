(defun vt/project-ansi-term ()
  "Start `ansi-term' in the current project's root directory."
  (interactive)
  (let* ((default-directory (project-root (project-current t))))
    (ansi-term "/usr/bin/fish")))

(provide 'vt-project)
