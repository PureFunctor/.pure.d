(require 'setup)

(setup-define :straight-x
  (lambda (recipe-or-name revision)
    (let* ((recipe (if (symbolp recipe-or-name)
                       (straight-recipes-retrieve recipe-or-name)
                     recipe-or-name)))
      (if (eq recipe 'nil)
	  `,(setup-quit (format "Cannot find recipe for '%s'" recipe-or-name))
        (let ((repo (plist-get (cdr recipe) :repo)))
          `(progn
             (let ((straight-current-profile 'pinned))
               (straight-use-package ',recipe)
               (add-to-list 'straight-x-pinned-packages
			    '(,(cadr (split-string repo "/" t)) . ,revision)))))))))

(setup-define :defer
  (lambda (&rest time)
    `(run-with-idle-timer ,(or (car time) 1) nil (lambda () (require ',(setup-get 'feature))))))

(setup-define :diminish
  (lambda (&rest modes)
    (let ((mode (setup-get 'mode))
          (body ()))
      (unless (memq mode modes)
        (push mode modes))
      (dolist (mode modes `(with-eval-after-load ',(setup-get 'feature) ,(macroexp-progn body)))
        (push `(diminish ',mode) body)))))

(provide 'vt-setup)
