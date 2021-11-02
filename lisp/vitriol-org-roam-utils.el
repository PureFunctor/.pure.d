(require 'org-roam)

(defconst vitriol/org-roam-longest-parent
  (+ 3 (-max (seq-map (lambda (x) (string-width (car x)))
                      (org-roam-db-query [:select title :from nodes :where (= level 0)])))))

(defconst vitriol/org-roam-longest-title
  (+ 3 (-max (seq-map (lambda (x) (string-width (car x)))
                      (org-roam-db-query [:select title :from nodes])))))

(defconst vitriol/org-roam-node-display-template
  (concat (propertize (format "${parent:%i}" vitriol/org-roam-longest-parent) 'face 'org-cite)
          "|"
          (format " ${title:%i} " vitriol/org-roam-longest-title)
          "|"
          (propertize " ${tags:20}" 'face 'org-tag)))

(defconst vitriol/org-roam-node-template-prefixes
      '(("parent" . "p:") ("tags" . "#") ("title" . "t:")))

(setq org-roam-node-template-prefixes vitriol/org-roam-node-template-prefixes)

(cl-defmethod org-roam-node-parent ((node org-roam-node))
  "Get the title of a NODE's parent NOdE."
  (let ((parent-title
         (org-roam-db-query
          [ :select title :from nodes :where (= file $s1) :and (= level 0) ] (org-roam-node-file node))))
    (car (car parent-title))))

(defun vitriol/org-roam-node-find ()
  "Find a node."
  (interactive)
  (let ((org-roam-node-display-template vitriol/org-roam-node-display-template))
    (org-roam-node-find)))

(defun vitriol/org-roam-node-insert ()
  "Insert a node."
  (interactive)
  (let ((org-roam-node-display-template vitriol/org-roam-node-display-template))
    (org-roam-node-insert)))

(provide 'vitriol-org-roam-utils)
