;;; pure-lang.el --- Configuration for language modes

;;; Commentary:
;;
;; This module contains configuration for languages
(require 'pure-setup)

;;; Code:

;;;; LSP
(straight-use-package 'lsp-mode)

(setq lsp-keymap-prefix "C-c l")
(setup (:require lsp-mode)
  (:option lsp-lens-enable nil
           lsp-headerline-breadcrumb-enable nil
           lsp-modeline-code-actions-enable nil
           lsp-modeline-diagnostics-enable nil)
  (:when-loaded
    (add-to-list 'lsp-language-id-configuration '(purs-mode . "purescript"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("purescript-language-server" "--stdio"))
      :activation-fn (lsp-activate-on "purescript")
      :server-id 'purescript-language-server))))

(straight-use-package 'company)

(setup (:require company-mode)
  (global-company-mode))

(straight-use-package 'yasnippet)

(setup (:require yasnippet)
  (yas-global-mode))

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

;;;; Rust
(straight-use-package 'rustic)
(setup (:require rustic)
  (:hook lsp))

;;;; Markdown
(straight-use-package 'markdown-mode)

(setup (:require markdown-mode))

(provide 'pure-lang)

;;; pure-lang.el ends here
