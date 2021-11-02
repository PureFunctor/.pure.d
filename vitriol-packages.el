(defun vitriol/packages/setup ()
  "Install and setup packages."
  (let ((straight-current-profile 'pinned))
    (straight-use-package 'setup)
    (add-to-list 'straight-x-pinned-packages
                 '("setup" . "5d281024dea8d670ef5d84f127c528f64e1e0865")))
  (require 'setup)
  
  (setup-define :straight-x
    (lambda (recipe-or-name revision)
      (let* ((recipe (if (symbolp recipe-or-name)
                         (straight-recipes-retrieve recipe-or-name)
                       (recipe-or-name)))
             )
	(if (eq recipe 'nil)
	    `,(setup-quit (format "Cannot find recipe for '%s'" recipe-or-name))
          (let ((repo (plist-get (cdr recipe) :repo)))
            `(progn
               (let ((straight-current-profile 'pinned))
                 (straight-use-package ',recipe)
                 (add-to-list 'straight-x-pinned-packages
			      '(,(cadr (split-string repo "/" t)) . ,revision))))))
	)
      )
    )

  (setup-define :defer
    (lambda (&rest time)
      `(run-with-idle-timer
        ,(or (car time) 1)
        nil
        (lambda ()
          (require ',(setup-get 'feature)))
        )
      )
    )

  (setup-define :diminish
    (lambda (&rest modes)
      (let ((mode (setup-get 'mode))
            (body ()))
        (unless (memq mode modes)
          (push mode modes))
        (dolist (mode modes (macroexp-progn body))
          (push `(diminish ',mode) body)))))
  
  (setup doom-themes
    (:straight-x doom-themes "3e6f5d9ce129ac6fc0f466eb6f5518593625578f")
    (load-theme 'doom-vibrant t))

  (setup diminish
    (:straight-x diminish "6ec6ebc391371418efc6c98d70b013f34af5a2ee")
    (:require diminish))
  
  (setup dashboard
    (:straight-x dashboard "a19868f2fb8f7fc4132b4e9bfac5cdd65f245181")
    (:bind "n" dashboard-next-line
           "p" dashboard-previous-line
           "f" dashboard-next-section
           "b" dashboard-previous-section)
    (:option dashboard-center-content t
             dashboard-banner-logo-title "Vitriol Emacs"
             dashboard-startup-banner "~/.pure.d/logo.png")
    (dashboard-setup-startup-hook))
  
  (setup consult
    (:straight-x consult "ebb62563127a4b9442148372f897efb7baef61d2")
    (:defer)
    (:global "C-s" consult-line
             "M-y" consult-yank-pop))

  (setup project
    (:require vitriol-project)
    (:with-map project-prefix-map
      (:bind "t" vitriol/project-ansi-term)))
  
  (setup perspective
    (:straight-x perspective "53348cea0f46655c4c072da5984f6a652726df4f")
    (:defer)
    (:option persp-mode-prefix-key (kbd "M-i")
             persp-initial-frame-name "Main")
    (:when-loaded
      (persp-mode)))

  (setup ace-window
    (:straight-x ace-window "c7cb315c14e36fded5ac4096e158497ae974bec9")
    (:defer)
    (:option aw-dispatch-always t
             aw-display-mode-overlay nil
             aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
    (:global "M-o" ace-window)
    (:when-loaded
      (ace-window-display-mode)))

  (setup which-key
    (:straight-x which-key "4790a14683a2f3e4f72ade197c78e4c0af1cdd4b")
    (:defer)
    (:when-loaded
      (which-key-mode)
      (:diminish)))

  (setup eldoc
    (:when-loaded (:diminish)))

  (setup font-lock
    (:when-loaded
      (set-face-attribute 'font-lock-builtin-face nil :slant 'italic)
      (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
      (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)))
  
  (setup selectrum
    (:straight-x selectrum "97693d0aea2c548197e9d1de3bdedf8e703775a4")
    (:defer)
    (:when-loaded
      (selectrum-mode +1)))
  
  (setup orderless
    (:straight-x orderless "62f71c34baca0b7d0adeab4a1c07d85ffcee80d9")
    (:defer)
    (:option completion-styles '(orderless)
             completion-category-defaults nil
             completion-category-overrides '((file (styles partial-completion)))))
  
  (setup marginalia
    (:straight-x marginalia "09d8ab38a5a4aa55a83968dc3e454d11fee05255")
    (:defer)
    (:when-loaded
      (marginalia-mode)))

  (setup diredfl
    (:straight-x diredfl "4ca32658aebaf2335f0368a0fd08f52eb1aee960")
    (:defer)
    (:hook-into dired-mode-hook))

  (setup org
    (:require vitriol-org-face-mode)
    (:defer)
    (:option org-directory "~/PureFunctor/Org/Agenda"
             org-agenda-files (list org-directory)
             org-hide-emphases-markers t
             org-edit-src-content-indentation 0)
    (:hook org-indent-mode
           vitriol/org-face-mode))

  (setup org-roam
    (setq org-roam-v2-ack t)    
    (:straight-x org-roam "3e47f198c7b6c3254944d98357e41840e5e1b102")
    (:defer)

    (:option org-roam-directory "~/PureFunctor/Org/Roam"
             org-roam-completion-everywhere t)
    
    (:require vitriol-org-roam-utils)
    (:global "C-c n l" org-roam-buffer-toggle
             "C-c n f" vitriol/org-roam-node-find
             "C-c n i" vitriol/org-roam-node-insert)

    (:when-loaded
      (org-roam-setup)))
  
  (setup magit
    (:straight-x magit "f44f6c14500476d918e9c01de8449edb20af4113")
    (:defer)
    (:option git-commit-summary-max-length 50)
    (:option transient-default-level 5)))

(defun vitriol/packages/entry ()
  "Entry point for `vitriol-packages.el'."
  (vitriol/packages/setup))

(provide 'vitriol-packages)
