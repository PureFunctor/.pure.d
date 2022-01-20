(defconst user-emacs-directory "~/.emacs.d/")

(defconst user-custom-file
  (concat user-emacs-directory "custom.el"))

(defconst user-backup-directory
  (concat user-emacs-directory "saves"))

(defconst user-snippet-directory
  (concat user-emacs-directory "snippets"))

(defconst user-wakatime-key
  (with-temp-buffer
    (insert-file-contents "~/Secrets/wakatime-api-key")
    (string-trim (buffer-string))))

(provide 'vt-constants)
