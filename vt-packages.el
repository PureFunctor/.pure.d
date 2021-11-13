(eval-and-compile
  (require 'vt-setup))

(setup (:require ibuffer)
  (:global "C-x C-b" ibuffer))

(setup (:global-unbind "C-z" "C-x C-z"))

(setup (:straight doom-themes)
  (:pin "emacs-doom-themes" "3e6f5d9ce129ac6fc0f466eb6f5518593625578f")
  (:require doom-themes)
  (load-theme 'doom-vibrant t)
  (disable-theme 'wombat))

(setup (:straight diminish)
  (:pin "diminish.el" "6ec6ebc391371418efc6c98d70b013f34af5a2ee"))
  
(setup (:straight dashboard)
  (:pin "emacs-dashboard" "a19868f2fb8f7fc4132b4e9bfac5cdd65f245181")
  (:bind "n" dashboard-next-line
         "p" dashboard-previous-line
         "f" dashboard-next-section
         "b" dashboard-previous-section)
  (:option dashboard-center-content t
           dashboard-banner-logo-title "Vitriol Emacs"
           dashboard-startup-banner "~/.pure.d/logo.png")
  (dashboard-setup-startup-hook))

(setup (:straight consult)
  (:pin "consult" "ebb62563127a4b9442148372f897efb7baef61d2")
  (:defer)
  (:global "C-s" consult-line
           "M-y" consult-yank-pop))

(setup (:require vt-project)
  (:with-map project-prefix-map
    (:bind "t" vt/project-ansi-term)))

(setup (:straight perspective)
  (:pin "perspective-el" "53348cea0f46655c4c072da5984f6a652726df4f")
  (:defer)
  (:option persp-mode-prefix-key (kbd "M-i")
           persp-initial-frame-name "Main")
  (:when-loaded
    (persp-mode)))

(setup (:straight ace-window)
  (:pin "ace-window" "c7cb315c14e36fded5ac4096e158497ae974bec9")
  (:defer)
  (:option aw-dispatch-always t
           aw-display-mode-overlay nil
           aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
  (:global "M-o" ace-window)
  (:when-loaded
    (ace-window-display-mode)))

(setup (:straight which-key)
  (:pin "emacs-which-key" "4790a14683a2f3e4f72ade197c78e4c0af1cdd4b")
  (:defer)
  (:diminish t)
  (:when-loaded
    (which-key-mode)))

(setup (:require eldoc)
  (:hook (apply-partially 'diminish 'eldoc-mode)))

(setup font-lock
  (:when-loaded
    (set-face-attribute 'font-lock-builtin-face nil :slant 'italic)
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)))

(setup (:straight selectrum)
  (:pin "selectrum" "97693d0aea2c548197e9d1de3bdedf8e703775a4")
  (:defer)
  (:when-loaded
    (selectrum-mode +1)))

(setup (:straight orderless)
  (:pin "orderless" "62f71c34baca0b7d0adeab4a1c07d85ffcee80d9")
  (:defer)
  (:option completion-styles '(orderless)
           completion-category-defaults nil
           completion-category-overrides '((file (styles partial-completion)))))

(setup (:straight marginalia)
  (:pin "marginalia" "09d8ab38a5a4aa55a83968dc3e454d11fee05255")
  (:defer)
  (:when-loaded
    (marginalia-mode)))

(setup (:straight diredfl)
  (:pin "diredfl" "4ca32658aebaf2335f0368a0fd08f52eb1aee960")
  (:defer)
  (:hook-into dired-mode-hook))

(setup org
  (:defer)
  (:option org-directory "~/PureFunctor/Org/Agenda"
           org-agenda-files (list org-directory)
           org-hide-emphases-markers t
           org-edit-src-content-indentation 0
           fill-column 100)
  (:hook org-indent-mode))

(setup (:straight org-superstar)
  (:pin "org-superstar-mode" "03be6c0a3081c46a59b108deb8479ee24a6d86c0")
  (:defer)
  (:option org-superstar-leading-bullet " ")
  (:hook-into org-mode-hook))

(setup (:straight visual-fill-column)
  (:defer)
  (:option visual-fill-column-width 120
           visual-fill-column-center-text t)
  (:hook-into org-mode-hook))

(setup (:straight
        (purs-mode :type git
                   :flavor melpa
                   :files (:defaults "purs-mode-pkg.el")
                   :host github
                   :repo "PureFunctor/purs-mode"))
  (:pin "purs-mode" "e747d4e636e620069176e661b8b0635014f140dc"))

(setup (:straight org-roam)
  (:pin "org-roam" "3e47f198c7b6c3254944d98357e41840e5e1b102")
  (:defer)

  (:option org-roam-directory "~/PureFunctor/Org/Roam"
           org-roam-completion-everywhere t
           org-roam-v2-ack t)

  (:global "C-c n l" org-roam-buffer-toggle
           "C-c n f" vt/org-roam-node-find
           "C-c n i" vt/org-roam-node-insert)

  (:when-loaded
    (:require vt-org-roam-utils)
    (org-roam-db-autosync-mode)))

(setup (:straight wakatime-mode)
  (:pin "wakatime-mode" "8dfe67c1581a0f3688c572dfdb5f8f71d3f874a0")
  (:defer)
  (:diminish t)
  (:option wakatime-api-key (with-temp-buffer
                              (insert-file-contents "~/Secrets/wakatime-api-key")
                              (string-trim (buffer-string))))
  (:when-loaded
    (global-wakatime-mode)))

(setup (:straight ws-butler)
  (:pin "ws-butler" "e3a38d93e01014cd47bf5af4924459bd145fd7c4")
  (:defer)
  (:diminish t)
  (:hook-into prog-mode))

(setup (:straight magit)
  (:pin "magit" "f44f6c14500476d918e9c01de8449edb20af4113")
  (:defer)
  (:option git-commit-summary-max-length 50)
  (:option transient-default-level 5))

(setup (:straight flycheck)
  (:pin "flycheck" "784f184cdd9f9cb4e3dbb997c09d93e954142842")
  (:global "M-p" flycheck-buffer
           "M-[" flycheck-previous-error
           "M-]" flycheck-next-error)
  (:option flycheck-check-syntax-automatically nil)
  (:hook-into purs-mode))

(setup (:require flycheck-spago))

(provide 'vt-packages)
