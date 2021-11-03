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

(setq straight-profiles
      '((nil . "~/.pure.d/versions/default.el")
        (pinned . "~/.pure.d/versions/pinned.el")))

(let ((straight-current-profile 'pinned))
  (straight-use-package 'setup)
  (add-to-list 'straight-x-pinned-packages
               '("setup" . "5d281024dea8d670ef5d84f127c528f64e1e0865")))
