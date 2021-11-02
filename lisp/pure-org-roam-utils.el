;;; pure-org-roam-utils.el --- My personal extensions to `org-roam'.
;;;
;;; -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:
(require 'org-roam)

(defconst pure/org-roam-longest-parent
  (+ 3 (-max (seq-map (lambda (x) (string-width (car x)))
                      (org-roam-db-query [:select title :from nodes :where (= level 0)])))))

(defconst pure/org-roam-longest-title
  (+ 3 (-max (seq-map (lambda (x) (string-width (car x)))
                      (org-roam-db-query [:select title :from nodes])))))

(defconst pure/org-roam-node-display-template
  (concat (propertize (format "${parent:%i}" pure/org-roam-longest-parent) 'face 'org-cite)
          "|"
          (format " ${title:%i} " pure/org-roam-longest-title)
          "|"
          (propertize " ${tags:20}" 'face 'org-tag)))

(defconst pure/org-roam-node-template-prefixes
      '(("parent" . "p:") ("tags" . "#") ("title" . "t:")))

(setq org-roam-node-template-prefixes pure/org-roam-node-template-prefixes)

(cl-defmethod org-roam-node-parent ((node org-roam-node))
  "Get the title of a NODE's parent NOdE."
  (let ((parent-title
         (org-roam-db-query
          [ :select title :from nodes :where (= file $s1) :and (= level 0) ] (org-roam-node-file node))))
    (car (car parent-title))))

(defun pure/org-roam-top-level-node-find ()
  "Find a node."
  (interactive)
  (let ((org-roam-node-display-template pure/org-roam-node-display-template))
    (org-roam-node-find)))

(defun pure/org-roam-node-insert ()
  "Insert a node."
  (interactive)
  (let ((org-roam-node-display-template pure/org-roam-node-display-template))
    (org-roam-node-insert)))

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
