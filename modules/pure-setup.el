;;; pure-setup.el --- Additions to setup.el

;;; Commentary:
;;
;; This module contains custom setup.el macros.

(require 'setup)

(setup-define :global-unbind
  (lambda (key)
    `(global-unset-key ,key))
  :documentation
  "Unbind a KEY globally."
  :repeatable
  :ensure `(kbd))

(provide 'pure-setup)

;;; pure-setup.el ends here
