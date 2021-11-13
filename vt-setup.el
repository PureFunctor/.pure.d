(eval-and-compile
  (require 'setup))

(setup-define :straight
  (lambda (recipe)
    `(unless (let ((straight-current-profile 'pinned))
               (straight-use-package ',recipe))
       ,(setup-quit)))
  :documentation
  "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
  :repeatable t
  :shorthand (lambda (sexp)
               (let ((recipe (cadr sexp)))
                 (if (consp recipe)
                     (car recipe)
                   recipe))))

(setup-define :pin
  (lambda (repository revision)
    `(add-to-list 'straight-x-pinned-packages
                  '(,repository . ,revision)))
  :documentation
  "Add a REPOSITORY and a REVISION to `straight-x-pinned-packages'."
  :repeatable t)

(setup-define :defer
  (lambda (&rest time)
    `(run-with-idle-timer ,(or (car time) 1) nil 'require ',(setup-get 'feature)))
  :documentation
  "Delay loading a FEATURE.")

(setup-define :diminish
  (lambda (self &rest modes)
    (let ((body ()))
      (when self
        (push `(diminish ',(setup-get 'mode)) body))
      (dolist (mode modes)
        (push `(diminish ',mode) body))
      `(with-eval-after-load ',(setup-get 'feature)
         ,(macroexp-progn body))))
  )
;;     (let ((self (setup-get 'mode))
;;           (body ()))
;;       (when (and (not (memq self modes)) (length> modes 1) (eq (car modes) t))
;;         (pop modes)
;;         (push self modes))
;;       (dolist (mode modes `(with-eval-after-load ',(setup-get 'feature) ,(macroexp-progn body)))
;;         (push `(diminish ',mode) body))))
;;   :documentation
;;   "Diminish MODES after loading the FEATURE.
;; A FEATURE's MODE is always included in MODES.")

(setup-define :global-unbind
  (lambda (key)
    `(global-unset-key ,key))
  :documentation
  "Globally unbind a KEY."
  :repeatable t
  :ensure '(kbd))

(provide 'vt-setup)
