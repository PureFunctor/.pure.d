(require 'face-remap)

(defface vitriol/org-default-face '((t . (:height 140))) "Custom default face for org-mode." :group 'org-faces)

(defface vitriol/org-variable-pitch nil "Custom variable-pitch face for org-mode." :group 'org-faces)

(defface vitriol/org-fixed-pitch nil "Custom fixed-pitch face for org-mode." :group 'org-faces)

(defun vitriol/org-face-mode (&optional arg)
  "Frontend for 'buffer-face-mode', similar to `variable-pitch-mode`."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode-invoke 'vitriol/org-variable-pitch (or arg t)
			   (called-interactively-p 'interactive)))

(provide 'vitriol-org-face-mode)
