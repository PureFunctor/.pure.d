(require 'org-roam)

(setq lexical-binding t)

(defun pure/org-roam-top-level-node-find ()
  "Filter out non-top-level nodes for find."
  (interactive)
  (org-roam-node-find
   nil
   nil
   (lambda (node)
     (= 0 (org-roam-node-level node)))))

;; TODO: Once my notes become sizeable enough,
;; I'll start implementing better narrowing
;; functions like these.

;; (defun pure/org-roam-filter-by-tags (tags)
;;   "Filter `org-roam' nodes by their `tags'."
;;   (lambda (node)
;;     (member tags (org-roam-node-tags node))))

;; (defun pure/org-roam-find-by-tag (tags)
;;   "Find `org-roam' nodes by their `tags'."
;;   (org-roam-node-find
;;    nil
;;    nil
;;    (pure/org-roam-filter-by-tags tags)))

;; (defun pure/org-roam-find-purescript ()
;;   "Find `org-roam' nodes related to `PureScript'."
;;   (interactive)
;;   (pure/org-roam-find-by-tag "PureScript"))

;; (defun pure/org-roam-find-practical ()
;;   "Find `org-roam' nodes related to practical implementations."
;;   (interactive)
;;   (pure/org-roam-find-by-tag "Practical"))

;; (defun pure/org-roam-find-theoretical ()
;;   "Find `org-roam' nodes related to theoretical implementations."
;;   (interactive)
;;   (pure/org-roam-find-by-tag "Theoretical"))

(provide 'pure-org-roam-utils)

;;; pure-org-roam-utils.el ends here
