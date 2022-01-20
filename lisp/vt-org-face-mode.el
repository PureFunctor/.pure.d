(require 'face-remap)

(defface vt/org-default-face '((t . (:height 120)))
  "Custom default face for org-mode."
  :group 'org-faces)

(defface vt/org-variable-pitch '((t . (:inherit vt/org-default-face)))
  "Custom variable-pitch face for org-mode."
  :group 'org-faces)

(defface vt/org-fixed-pitch '((t . (:inherit vt/org-default-face)))
  "Custom fixed-pitch face for org-mode."
  :group 'org-faces)

(defun vt/org-face-mode (&optional arg)
  "Frontend for 'buffer-face-mode', similar to `variable-pitch-mode`."
  (interactive (list (or current-prefix-arg 'toggle)))
  (set-face-attribute
   'vt/org-variable-pitch nil
   :family "Noto Sans")
  (set-face-attribute
   'vt/org-fixed-pitch nil
   :family "Mononoki")
  (set-face-attribute
   'org-block nil
   :inherit 'vt/org-fixed-pitch)
  (set-face-attribute
   'org-level-1 nil
   :height 2.0)
  (set-face-attribute
   'org-level-2 nil
   :height 1.8)
  (set-face-attribute
   'org-level-3 nil
   :height 1.6)
  (buffer-face-mode-invoke 'vt/org-variable-pitch (or arg t)
			   (called-interactively-p 'interactive)))

(provide 'vt-org-face-mode)
