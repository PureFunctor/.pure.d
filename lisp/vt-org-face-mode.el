(require 'face-remap)

(defface vt/org-default-face '((t . (:height 140))) "Custom default face for org-mode." :group 'org-faces)

(defface vt/org-variable-pitch nil "Custom variable-pitch face for org-mode." :group 'org-faces)

(defface vt/org-fixed-pitch nil "Custom fixed-pitch face for org-mode." :group 'org-faces)

(defun vt/org-face-mode (&optional arg)
  "Frontend for 'buffer-face-mode', similar to `variable-pitch-mode`."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode-invoke 'vt/org-variable-pitch (or arg t)
			   (called-interactively-p 'interactive)))

(provide 'vt-org-face-mode)
