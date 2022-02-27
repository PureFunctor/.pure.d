;;; pure-completions.el --- Completions-related configuration

;;; Commentary:
;;
;; This module contains configuration regarding completion interfaces.
(require 'pure-setup)

;;; Code:

;;; Consult
(straight-use-package 'consult)

(setup (:require consult)
  (:global "C-x b" consult-buffer
	   "C-c r f" consult-recent-file
	   "C-c r g" consult-ripgrep
           "M-y" consult-yank-pop))

;;;; Selectrum
(straight-use-package 'selectrum)

(setup (:require selectrum)
  (:option selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (selectrum-mode +1))

;;;; Orderless
(straight-use-package 'orderless)

(setup (:require orderless)
  (:option completion-styles '(orderless)
           completion-category-defaults nil
           completion-category-overrides '((file (styles partial-completion)))
           orderless-skip-highlighting (lambda () selectrum-is-active)))

;;;; Marginalia
(straight-use-package 'marginalia)

(setup (:require marginalia)
  (:with-map minibuffer-local-map
    (:bind "M-a" marginalia-cycle))
  (marginalia-mode))

(provide 'pure-completions)

;;; pure-completions.el ends here
