(require 'face-remap)

(defface pure/org-default-face '((t . (:height 140))) "Custom default face for org-mode." :group 'org-faces)

(defface pure/org-variable-pitch nil "Custom variable-pitch face for org-mode." :group 'org-faces)

(defface pure/org-fixed-pitch nil "Custom fixed-pitch face for org-mode." :group 'org-faces)

(defun pure/org-face-mode (&optional arg)
  "Frontend for 'buffer-face-mode', similar to `variable-pitch-mode`."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode-invoke 'pure/org-variable-pitch (or arg t)
			   (called-interactively-p 'interactive)))

(provide 'pure-org-face-mode)
;;; pure-org-face-mode.el ends here
