(require 'vt-builtins)
(require 'vt-packages)

(defun vt/config/entry ()
  "Load config"
  (vt/builtins/entry)
  (vt/packages/entry))

(provide 'vt-config)
