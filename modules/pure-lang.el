;;; pure-lang.el --- Configuration for language modes

;;; Commentary:
;;
;; This module contains configuration for languages
(require 'pure-setup)

;;; Code:

;;;; LSP
(straight-use-package 'lsp-mode)

(setq lsp-keymap-prefix "M-l")
(setup (:require lsp-mode)
  (:option lsp-lens-enable nil)
  (:when-loaded
    (add-to-list 'lsp-language-id-configuration '(purs-mode . "purescript"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("purescript-language-server" "--stdio"))
      :activation-fn (lsp-activate-on "purescript")
      :server-id 'purescript-language-server))))

;;;; Flycheck
(straight-use-package 'flycheck)

(setup (:require flycheck)
  (:global "M-p" flycheck-buffer
           "M-[" flycheck-previous-error
           "M-]" flycheck-next-error)
  (:option flycheck-check-syntax-automatically '(save)))

;;;; OCaml
(straight-use-package 'tuareg)

(setup (:require tuareg)
  (:hook lsp flycheck-mode))

;;;; Haskell
(straight-use-package 'haskell-mode)

(setup (:require haskell-mode)
  (:unbind "M-[" "M-]")
  (:bind "M-]" 'haskell-goto-next-error
         "M-[" 'haskell-goto-prev-error)
  (:hook interactive-haskell-mode)
  (setup (:require haskell-interactive-mode)
    (:option haskell-process-use-presentation-mode t)
    (:unbind "M-[" "M-]")
    (:bind "M-]" 'haskell-interactive-mode-error-forward
           "M-[" 'haskell-interactive-mode-error-backward)))

;;;; PureScript
(add-to-list 'load-path "~/Vitriol/purs-mode")
(setup (:require purs-mode)
  (:hook lsp purs-indentation-mode))

;;;; Markdown
(straight-use-package 'markdown-mode)

(setup (:require markdown-mode))

(provide 'pure-lang)

;;; pure-lang.el ends here
