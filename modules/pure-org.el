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
  (:hook org-indent-mode variable-pitch-mode))

;;;; Org Superstar
(straight-use-package 'org-superstar)

(setup (:require org-superstar)
  (:option org-superstar-leading-bullet " "
           org-superstar-headline-bullets-list '(?∃ ?∀ ?⊢))
  (:hook-into org-mode-hook))

;;;; Ox Hugo
(straight-use-package 'ox-hugo)

(with-eval-after-load 'ox
  (require 'ox-hugo))

;;;; Visual Fill Column
(straight-use-package 'visual-fill-column)

(setup (:require visual-fill-column)
  (:option visual-fill-column-width 105
           visual-fill-column-center-text t)
  (:hook-into org-mode-hook))

;;;; Org Roam
(straight-use-package 'org-roam)

(setup (:require org-roam)
  (:option org-roam-directory "~/PureFunctor/Org/Roam/roam"
           org-roam-completion-everywhere t
           org-roam-v2-ack t)

  (defconst my/org-roam-longest-parent
    (+ 3 (-max (seq-map (lambda (x) (string-width (car x)))
                        (org-roam-db-query [:select title :from nodes :where (= level 0)])))))

  (defconst my/org-roam-longest-title
    (+ 3 (-max (seq-map (lambda (x) (string-width (car x)))
                        (org-roam-db-query [:select title :from nodes])))))

  (defconst my/org-roam-node-display-template
    (concat (propertize (format "${parent:%i}" my/org-roam-longest-parent) 'face 'org-cite)
            "|"
            (format " ${title:%i} " my/org-roam-longest-title)
            "|"
            (propertize " ${tags:20}" 'face 'org-tag)))

  (defconst my/org-roam-node-template-prefixes
    '(("parent" . "p:") ("tags" . "#") ("title" . "t:")))

  (setq org-roam-node-template-prefixes my/org-roam-node-template-prefixes)

  (cl-defmethod org-roam-node-parent ((node org-roam-node))
    "Get the title of a NODE's parent NOdE."
    (let ((parent-title
           (org-roam-db-query
            [ :select title :from nodes :where (= file $s1) :and (= level 0) ] (org-roam-node-file node))))
      (car (car parent-title))))

  (defun my/org-roam-node-find ()
    "Find a node."
    (interactive)
    (let ((org-roam-node-display-template my/org-roam-node-display-template))
      (org-roam-node-find)))

  (defun my/org-roam-node-insert ()
    "Insert a node."
    (interactive)
    (let ((org-roam-node-display-template my/org-roam-node-display-template))
      (org-roam-node-insert)))

  (:global "C-c n l" org-roam-buffer-toggle
           "C-c n f" my/org-roam-node-find
           "C-c n i" my/org-roam-node-insert)

  (org-roam-db-autosync-mode))

(provide 'pure-org)

;;; pure-org.el ends here
