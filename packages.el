(defun packages/bootstrap ()
  "Bootstrap `straight.el`."
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package)
  (straight-use-package 'diminish))

(defun packages/extra ()
  "Load extra."
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  (diminish 'variable-pitch-mode)
  (diminish 'visual-line-mode))

(defun packages/theme ()
  "Load theme."
  (use-package doom-themes
    :straight t
    :config
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t
          doom-horizon-brighter-comments t)
    (load-theme 'doom-vibrant t)
    (doom-themes-org-config))

  (use-package all-the-icons
    :straight t)

  (use-package all-the-icons-ivy-rich
    :straight t
    :disabled
    :after ivy-rich
    :config
    (all-the-icons-ivy-rich-mode))

  (use-package diredfl
    :straight t
    :diminish
    :hook (dired-mode . diredfl-mode))

  (use-package rainbow-delimiters
    :straight t
    :hook
    (prog-mode . rainbow-delimiters-mode))

  (use-package dashboard
    :straight t
    :bind ( :map dashboard-mode-map
                 ("n" . 'dashboard-next-line)
                 ("p" . 'dashboard-previous-line)
                 ("f" . 'dashboard-next-section)
                 ("b" . 'dashboard-previous-section))
    :custom
    (dashboard-center-content t)
    (dashboard-banner-logo-title "PureFunctor")
    (dashboard-startup-banner "~/.emacs.d/logo.png")
    :config
    (dashboard-setup-startup-hook))

  (use-package org-superstar
    :straight t
    :after org
    :custom
    (org-superstar-leading-bullet ?\s)
    :hook
    (org-mode . org-superstar-mode))

  (use-package visual-fill-column
    :straight t
    :custom
    (visual-fill-column-center-text t)
    (visual-fill-column-width 120)
    :hook
    (org-mode . visual-fill-column-mode)))

(defun packages/tools ()
  "Load tools."
  (use-package consult
    :straight t
    :bind
    ("C-c c r" . consult-ripgrep)
    ("C-c c f" . consult-find)
    ("C-c c o" . consult-outline)
    ("C-s" . consult-line)
    :custom
    (consult-project-root-function
     (lambda ()
       (when-let (project (project-current))
         (car (project-roots project)))))
    (consult-ripgrep-args
     "rg . --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --hidden -g !.git/"))

  (use-package marginalia
    :straight t
    :init
    (marginalia-mode))

  (use-package orderless
    :straight t
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

  (use-package selectrum
    :straight t
    :config
    (selectrum-mode +1))

  (use-package all-the-icons-completion
    :straight t
    :config
    (all-the-icons-completion-mode))

  (use-package company
    :straight t
    :custom
    (company-idle-delay 2.5)
    :diminish
    :bind ( :map company-mode-map
                 ("C-." . 'company-complete))
    :config
    (global-company-mode))

  (use-package flycheck
    :straight t
    :bind
    ("M-p" . 'flycheck-buffer)
    ("M-[" . 'flycheck-previous-error)
    ("M-]" . 'flycheck-next-error)
    :custom
    (flycheck-check-syntax-automatically nil)
    :hook
    (haskell-mode . flycheck-mode)
    (purescript-mode . flycheck-mode))

  (use-package consult-flycheck
    :straight t
    :after flycheck
    :bind
    ("M-P" . 'consult-flycheck))

  (use-package flycheck-spago
    :after flycheck
    :config
    (push 'spago flycheck-checkers))

  (use-package magit
    :straight t
    :defer t
    :custom
    (fill-column 72)
    (git-commit-summary-max-length 50)
    (transient-default-level 5)
    :bind
    ("C-c g" . 'magit))
  
  (use-package perspective
    :straight t
    :custom
    (persp-state-default-file "~/.cache/.persp")
    (persp-initial-frame-name "Main")
    :init
    (setq persp-mode-prefix-key (kbd "M-i"))
    :config
    (persp-mode))

  (use-package embark
    :straight t
    :bind
    ("M-S-\\" . 'embark-dwim)
    ("M-\\" . 'embark-act)
    ("C-h B" . 'embark-bindings)
    :init
    (setq prefix-help-command #'embark-prefix-help-command))

  (use-package project
    :straight t)

  (use-package vterm
    :straight t
    :defer 0
    :init
    (add-hook 'vterm-exit-functions
              (lambda (_ _)
                (let* ((buffer (current-buffer))
                       (window (get-buffer-window buffer)))
                  (when (not (one-window-p))
                    (delete-window window))
                  (kill-buffer buffer)))))

  (use-package org
    :straight t
    :init
    (require 'pure-org-face-mode)
    :custom
    (org-directory "~/PureFunctor/Org/Agenda")
    (org-agenda-files (list org-directory))
    (org-hide-emphasis-markers t)
    (org-edit-src-content-indentation 0)
    :custom-face
    (org-block ((t (:height 98 :inherit pure/org-fixed-pitch))))
    (org-code ((t (:height 120 :inherit pure/org-fixed-pitch))))
    (org-document-info-keyword ((t (:height 120 :inherit pure/org-fixed-pitch))))
    (org-drawer ((t (:height 120 :inherit pure/org-fixed-pitch))))
    (org-meta-line ((t :height 120 :inherit pure/org-fixed-pitch)))
    (org-property-value ((t (:height 120 :inherit pure/org-fixed-pitch))))
    (org-special-keyword ((t (:height 120 :inherit pure/org-fixed-pitch))))
    :config
    (set-face-attribute
     'pure/org-variable-pitch nil
     :font "Cormorant Garamond"
     :height 'unspecified
     :inherit 'pure/org-default-face)
    (set-face-attribute
     'pure/org-fixed-pitch nil
     :font "Fira Code"
     :height 'unspecified
     :inherit 'pure/org-default-face)
    (set-face-attribute
     'org-level-1 nil
     :height 1.75)
    (set-face-attribute
     'org-level-2 nil
     :height 1.50)
    (set-face-attribute
     'org-level-3 nil
     :height 1.25)
    :hook
    (org-mode . org-indent-mode)
    (org-mode . pure/org-face-mode)
    :bind
    ("C-c o a" . org-agenda))

  (use-package org-roam
    :straight t
    :init
    (setq org-roam-v2-ack t)
    :config
    (org-roam-setup)
    (require 'pure-org-roam-utils)
    :custom
    (org-roam-directory "~/PureFunctor/Org/Roam/")
    (org-roam-completion-everywhere t)
    :bind
    ("C-c n l" . org-roam-buffer-toggle)
    ("C-c n f" . pure/org-roam-top-level-node-find)
    ("C-c n i" . pure/org-roam-node-insert))

  (use-package org-roam-ui
    :straight
    ( :host github
      :repo "org-roam/org-roam-ui"
      :branch "main"
      :files ("*.el" "out")
    )
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)))

(defun packages/utils ()
  "Load utils."
  (use-package avy
    :straight t
    :defer 0
    :bind
    ("C-," . 'avy-goto-char))

  (use-package popper
    :straight t
    :bind (("C-`"   . popper-toggle-latest)
           ("M-`"   . popper-cycle)
           ("C-M-`" . popper-toggle-type))
    :init
    (setq popper-reference-buffers
          '("\\*Messages\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            "*idris-repl*"
            "*idris-info*"
            "*idris-holes*"
            "*idris-process*"
            "*idris-notes*"
            "*Embark Actions*"
            help-mode
            compilation-mode))
    (popper-mode +1)
    (popper-echo-mode +1))

  (use-package ace-window
    :straight t
    :defer 0
    :bind
    ("M-o" . 'ace-window))
  
  (use-package rotate
    :straight t
    :defer 0
    :bind
    ("C-c w m h" . 'rotate:main-horizontal)
    ("C-c w m v" . 'rotate:main-vertical)
    ("C-c w t" . 'rotate:tiled)
    ("C-c w s" . 'rotate-window)
    ("C-c w v" . 'split-window-right)
    ("C-c w h" . 'split-window-below)
    ("C-c w d" . 'delete-window))

  (use-package which-key
    :straight t
    :defer 0
    :diminish
    :config
    (which-key-mode))

  (use-package ws-butler
    :straight t
    :defer 0
    :diminish
    :config
    (ws-butler-global-mode))

  (use-package undo-tree
    :straight t
    :defer 0
    :diminish
    :config (global-undo-tree-mode))

  (use-package wakatime-mode
    :straight t
    :defer 0
    :diminish
    :config
    (setq wakatime-api-key
          (with-temp-buffer
            (insert-file-contents "~/Secrets/wakatime-api-key")
            (string-trim (buffer-string))))
    (global-wakatime-mode)))

(defun packages/major ()
  "Misc major modes."
  (use-package markdown-mode
    :straight t
    :hook
    (markdown-mode . visual-line-mode)))

(defun packages/langs ()
  "Load langs."
  (use-package purescript-mode
    :straight t
    :hook
    (purescript-mode . turn-on-purescript-indentation)
    (purescript-indentation-mode . (lambda () (diminish 'purescript-indentation-mode)))
    ;; (purescript-mode . turn-on-purescript-unicode-input-method)
    :mode
    ("\\.purs$" . 'purescript-mode)
    :config
    (modify-syntax-entry ?\\ "w" purescript-mode-syntax-table))

  (use-package psc-ide
    :straight t
    :bind
    ( :map purescript-mode-map
      ("C-c /" . 'psc-ide-flycheck-insert-suggestion))
    :after purescript-mode
    :hook
    (purescript-mode . psc-ide-mode))
  
  (use-package haskell-mode
    :straight t
    :defer t
    :custom
    (haskell-process-use-presentation-mode t)
    :config
    (defun haskell-mode-setup ()
      "Set up `haskell-mode`"
      (interactive-haskell-mode)
      (setq-default flycheck-disabled-checkers '(haskell-ghc haskell-stack-ghc))
      (define-key interactive-haskell-mode-map (kbd "C-c C-r") 'haskell-process-restart)
      (define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
      (define-key interactive-haskell-mode-map (kbd "C-c C-f") 'haskell-mode-find-uses)
      (define-key interactive-haskell-mode-map (kbd "C-c C-d") 'haskell-mode-jump-to-def)
      (define-key interactive-haskell-mode-map (kbd "C-c C-b") 'haskell-process-load-file)
      (define-key interactive-haskell-mode-map (kbd "M-]") 'haskell-goto-next-error)
      (define-key interactive-haskell-mode-map (kbd "M-[") 'haskell-goto-prev-error)
      (define-key haskell-interactive-mode-map (kbd "M-[") 'haskell-interactive-mode-error-forward)
      (define-key haskell-interactive-mode-map (kbd "M-]") 'haskell-interactive-mode-error-backward)
      (push 'company-ghci company-backends))
    (add-hook 'haskell-mode-hook 'haskell-mode-setup))

  (use-package company-ghci
    :straight t
    :after haskell-mode)

  (use-package emmet-mode
    :straight t
    :hook
    (mhtml-mode . emmet-mode))

  (use-package rustic
    :straight t
    :defer t)

  (use-package eglot
    :straight t
    :defer t
    :custom
    (rustic-lsp-client 'eglot))
    :custom
    (rustic-lsp-client 'eglot)))

(defun packages/entry ()
  "Entry point for `packages.el`"
  (packages/bootstrap)
  (packages/extra)
  (packages/theme)
  (packages/tools)
  (packages/utils)
  (packages/langs)
  (packages/major))

(provide 'packages)
