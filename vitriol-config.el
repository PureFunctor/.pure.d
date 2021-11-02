(require 'vitriol-builtins)
(require 'vitriol-packages)

(defun vitriol/config/entry ()
  "Load config"
  (vitriol/builtins/entry)
  (vitriol/packages/entry))

(provide 'vitriol-config)
