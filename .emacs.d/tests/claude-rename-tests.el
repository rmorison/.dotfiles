;;; claude-rename-tests.el --- Renaming a session and its buffer -*- lexical-binding: t -*-

;;; Commentary:

;; A Claude session has two names.  These check they stay together, and that
;; the command refuses rather than forcing when they cannot.

;;; Code:

(require 'ert)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defvar claude-rename-tests--loaded nil)

(defun claude-rename-tests--setup ()
  (unless claude-rename-tests--loaded
    (cfg-test-require-package "claude-code" 'claude-code)
    (cfg-test-eval-init-region
     ";; Rename a Claude session and its Emacs buffer together"
     "(kbd \"N\") #'my/claude-code-rename")
    (setq claude-rename-tests--loaded t)))

(ert-deftest rename/replaces-the-instance-keeping-the-directory ()
  (claude-rename-tests--setup)
  (should (equal (my/claude-code--rename-to "*claude:.dotfiles:old*" "fixit")
                 "*claude:.dotfiles:fixit*")))

(ert-deftest rename/promotes-an-unnamed-session ()
  (claude-rename-tests--setup)
  (should (equal (my/claude-code--rename-to "*claude:.dotfiles*" "fixit")
                 "*claude:.dotfiles:fixit*")))

(ert-deftest rename/result-still-parses-upstream ()
  "The renamed buffer must stay recognisable to claude-code.el, or its own
buffer-finding commands stop seeing the session."
  (claude-rename-tests--setup)
  (should (equal (claude-code--extract-instance-name-from-buffer-name
                  (my/claude-code--rename-to "*claude:.dotfiles:old*" "fixit"))
                 "fixit")))

(ert-deftest rename/refuses-a-name-with-a-colon-in-the-directory ()
  "Regression: upstream's pattern cannot split a TRAMP default-directory, so
rewriting one silently truncated it into a name that no longer resolved."
  (claude-rename-tests--setup)
  (should-error (my/claude-code--rename-to "*claude:/ssh:host:/home/x/:old*" "n"))
  (should-error (my/claude-code--rename-to "*claude:/ssh:host:/home/x/*" "n")))

(ert-deftest rename/rejects-names-it-cannot-round-trip ()
  (claude-rename-tests--setup)
  (dolist (bad '("" "   " "a*b" "a:b" "a\nb"))
    (should-error (my/claude-code--check-instance-name bad)))
  (should (progn (my/claude-code--check-instance-name "fixit") t)))

(ert-deftest rename/clears-the-input-line-before-the-command ()
  "Regression: the slash command used to be appended to whatever was already
typed, submitting the lot as an ordinary prompt while the buffer was renamed."
  (claude-rename-tests--setup)
  (cfg-test-with-buffer "*claude:p:old*" (cfg-test-composer "old")
    (should (equal (cfg-test-capturing-sends
                     (my/claude-code-rename "fixit" buf))
                   (list (kbd "ESC") "/rename fixit" (kbd "RET"))))
    (should (equal (buffer-name buf) "*claude:p:fixit*"))))

(ert-deftest rename/refuses-a-collision-and-sends-nothing ()
  "The rename happens before the send, so a refusal leaves both names alone."
  (claude-rename-tests--setup)
  (cfg-test-with-buffer "*claude:p:mine*" (cfg-test-composer "mine")
    (let ((other (get-buffer-create "*claude:p:taken*")))
      (unwind-protect
          (progn
            (should (equal (cfg-test-capturing-sends
                             (ignore-errors (my/claude-code-rename "taken" buf)))
                           nil))
            (should (equal (buffer-name buf) "*claude:p:mine*")))
        (kill-buffer other)))))

(ert-deftest rename/renaming-to-the-current-name-is-allowed ()
  (claude-rename-tests--setup)
  (cfg-test-with-buffer "*claude:p:same*" (cfg-test-composer "same")
    (should (equal (cfg-test-capturing-sends (my/claude-code-rename "same" buf))
                   (list (kbd "ESC") "/rename same" (kbd "RET"))))
    (should (equal (buffer-name buf) "*claude:p:same*"))))

(provide 'claude-rename-tests)
;;; claude-rename-tests.el ends here
