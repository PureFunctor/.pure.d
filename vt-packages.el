(eval-and-compile
  (require 'vt-setup))

(setup (:require ibuffer)
  (:global "C-x C-b" ibuffer))

(setup (:global-unbind "C-z" "C-x C-z"))

(setup (:straight doom-themes)
  (:require doom-themes)
  (load-theme 'doom-horizon t)
  (disable-theme 'wombat))

(setup (:straight diminish))
  
(setup (:straight dashboard)
  (:bind "n" dashboard-next-line
         "p" dashboard-previous-line
         "f" dashboard-next-section
         "b" dashboard-previous-section)
  (:option dashboard-center-content t
           dashboard-banner-logo-title "Vitriol Emacs"
           dashboard-startup-banner "~/.pure.d/logo.png")
  (dashboard-setup-startup-hook))

(setup (:straight consult)
  (:global "C-s" consult-line
           "M-y" consult-yank-pop))

(setup (:straight project)
  (setup (:require vt-project)
    (:with-map project-prefix-map
      (:bind "t" vt/project-ansi-term))))

(setup (:straight perspective)
  (:option persp-mode-prefix-key (kbd "M-i")
           persp-initial-frame-name "Main")
  (persp-mode))

(setup (:straight ace-window)
  (:option aw-dispatch-always t
           aw-display-mode-overlay nil
           aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
  (:global "M-o" ace-window)
  (ace-window-display-mode))

(setup (:straight which-key)
  (:diminish t)
  (which-key-mode))

(setup (:require eldoc)
  (:hook (apply-partially 'diminish 'eldoc-mode)))

(setup font-lock
  (:when-loaded
    (set-face-attribute 'font-lock-builtin-face nil :slant 'italic)
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)))

(setup (:straight selectrum)
  (:option selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (selectrum-mode +1))

(setup (:straight orderless)
  (:option completion-styles '(orderless)
           completion-category-defaults nil
           completion-category-overrides '((file (styles partial-completion)))
           orderless-skip-highlighting (lambda () selectrum-is-active)))

(setup (:straight marginalia)
  (marginalia-mode)
  (:with-map minibuffer-local-map
    (:bind "M-a" marginalia-cycle)))

(setup (:straight diredfl)
  (:hook-into dired-mode-hook))

(setup (:straight org)
  (:option org-directory "~/PureFunctor/Org/Agenda"
           org-agenda-files (list org-directory)
           org-hide-emphasis-markers t
           org-edit-src-content-indentation 0
           fill-column 100)
  (:when-loaded
    (add-to-list 'org-src-lang-modes '("purescript" . purs)))
  (setup (:require vt-org-face-mode))
  (:hook org-indent-mode vt/org-face-mode
         (lambda () (fringe-mode 8))))

(setup (:straight org-superstar)
  (:option org-superstar-leading-bullet " "
           org-superstar-headline-bullets-list '(?▶ ?▷))
  (:hook-into org-mode-hook))

(setup (:straight haskell-mode)
  (:unbind "M-[" "M-]")
  (:bind "M-]" 'haskell-goto-next-error
         "M-[" 'haskell-goto-prev-error)
  (:hook interactive-haskell-mode)
  (setup (:require haskell-interactive-mode)
    (:option haskell-process-use-presentation-mode t)
    (:unbind "M-[" "M-]")
    (:bind "M-]" 'haskell-interactive-mode-error-forward
           "M-[" 'haskell-interactive-mode-error-backward)))

(setup (:straight popper)
  (:bind "M-`" 'popper-cycle)
  (:option popper-reference-buffers
           '("\\*Messages\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             help-mode
             compilation-mode
             haskell-interactive-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(setup (:straight org-roam)
  (:option org-roam-directory "~/PureFunctor/Org/Roam"
           org-roam-completion-everywhere t
           org-roam-v2-ack t)
  (:global "C-c n l" org-roam-buffer-toggle
           "C-c n f" vt/org-roam-node-find
           "C-c n i" vt/org-roam-node-insert)
  (:require vt-org-roam-utils)
  (org-roam-db-autosync-mode))

;; (setup (:straight wakatime-mode)
;;   (:diminish t)
;;   (:option wakatime-api-key user-wakatime-key)
;;   (global-wakatime-mode))

(setup (:straight ws-butler)
  (:diminish t)
  (:hook-into prog-mode))

(setup (:straight magit)
  (:defer)
  (:option git-commit-summary-max-length 50)
  (:option transient-default-level 5))

(setup (:straight forge))

(setup (:straight flycheck)
  (:global "M-p" flycheck-buffer
           "M-[" flycheck-previous-error
           "M-]" flycheck-next-error)
  (:option flycheck-check-syntax-automatically '(save))
  (setup (:require flycheck-spago)))

(setup (:straight lsp-mode)
  (:option lsp-keymap-prefix "M-l"
           lsp-lens-enable nil)
  (:when-loaded
    (add-to-list 'lsp-language-id-configuration '(purs-mode . "purescript"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("purescript-language-server" "--stdio"))
      :activation-fn (lsp-activate-on "purescript")
      :server-id 'purescript-language-server))))

(setup (:straight tuareg)
  (:hook lsp flycheck-mode))

(add-to-list 'load-path "~/Vitriol/purs-mode")
(setup (:require purs-mode)
  (:hook lsp flycheck-mode purs-indentation-mode)
  (:bind "M-g M-n" flymake-goto-next-error
         "M-g M-p" flymake-goto-prev-error))

(setup (:straight markdown-mode))

(provide 'vt-packages)
