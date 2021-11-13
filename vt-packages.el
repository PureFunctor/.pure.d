(eval-and-compile
  (require 'vt-setup))

(setup (:require ibuffer)
  (:global "C-x C-b" ibuffer))

(setup (:global-unbind "C-z" "C-x C-z"))

(setup (:straight doom-themes)
  (:require doom-themes)
  (load-theme 'doom-vibrant t)
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
  (:defer)
  (:global "C-s" consult-line
           "M-y" consult-yank-pop))

(setup (:require vt-project)
  (:with-map project-prefix-map
    (:bind "t" vt/project-ansi-term)))

(setup (:straight perspective)
  (:defer)
  (:option persp-mode-prefix-key (kbd "M-i")
           persp-initial-frame-name "Main")
  (:when-loaded
    (persp-mode)))

(setup (:straight ace-window)
  (:defer)
  (:option aw-dispatch-always t
           aw-display-mode-overlay nil
           aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
  (:global "M-o" ace-window)
  (:when-loaded
    (ace-window-display-mode)))

(setup (:straight which-key)

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
  (:defer)
  (:when-loaded
    (selectrum-mode +1)))

(setup (:straight orderless)
  (:defer)
  (:option completion-styles '(orderless)
           completion-category-defaults nil
           completion-category-overrides '((file (styles partial-completion)))))

(setup (:straight marginalia)
  (:defer)
  (:when-loaded
    (marginalia-mode)))

(setup (:straight diredfl)
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
                   :repo "PureFunctor/purs-mode")))

(setup (:straight org-roam)
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
  (:defer)
  (:diminish t)
  (:option wakatime-api-key (with-temp-buffer
                              (insert-file-contents "~/Secrets/wakatime-api-key")
                              (string-trim (buffer-string))))
  (:when-loaded
    (global-wakatime-mode)))

(setup (:straight ws-butler)
  (:defer)
  (:diminish t)
  (:hook-into prog-mode))

(setup (:straight magit)
  (:defer)
  (:option git-commit-summary-max-length 50)
  (:option transient-default-level 5))

(setup (:straight flycheck)
  (:global "M-p" flycheck-buffer
           "M-[" flycheck-previous-error
           "M-]" flycheck-next-error)
  (:option flycheck-check-syntax-automatically nil)
  (:hook-into purs-mode))

(setup (:require flycheck-spago))

(provide 'vt-packages)
