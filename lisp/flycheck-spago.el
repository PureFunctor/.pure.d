;;; flymake-spago.el --- Spago backend for Flymake -*- lexical-binding: t -*-

(require 'flycheck)

(defvar flycheck-spago-dhall-file "tests.dhall")

(defun flycheck-spago-working-directory (_checker)
  "Find spago.dhall in the parent directory of the current buffer."
  (locate-dominating-file (or buffer-file-name default-directory) "spago.dhall"))

(defun flycheck-parse-purs-json (message checker buffer)
  "Create `flycheck-error' objects using JSON output from `purs'."
  (let (output)
    (let-alist message

      (dolist (warning .warnings)
        (push (flycheck-parse-purs-diagnostic checker buffer warning 'warning) output))

      (dolist (_error .errors)
        (push (flycheck-parse-purs-diagnostic checker buffer _error 'error) output)))

    (nreverse output)))

(defun flycheck-parse-purs-diagnostic (checker buffer diagnostic severity)
  "Create a `flycheck-error' object given a `purs' diagnostic."
  (let-alist diagnostic
    (flycheck-error-new-at
     .position.startLine
     .position.startColumn
     severity
     .message
     :id .error-code
     :checker checker
     :buffer buffer
     :filename .filename
     :group 'group
     :end-line .position.endLine
     :end-column .position.endColumn)))

(defun flycheck-spago-error-parser (output checker buffer)
  (seq-mapcat (lambda (message)
                (flycheck-parse-purs-json message checker buffer))
              (flycheck-parse-json output)))

(flycheck-define-checker spago
  "Spago backend for `flycheck-mode'."
  :command ("spago" "-x" (eval flycheck-spago-dhall-file) "-q" "build" "-u" "--censor-lib --json-errors --stash")
  :modes purs-mode
  :working-directory flycheck-spago-working-directory
  :error-parser flycheck-spago-error-parser)

(add-to-list 'flycheck-checkers 'spago)

(provide 'flycheck-spago)
