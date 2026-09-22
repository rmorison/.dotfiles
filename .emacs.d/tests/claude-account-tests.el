;;; claude-account-tests.el --- Per-directory Claude account selection -*- lexical-binding: t -*-

;;; Commentary:

;; Every bug this has had produced plausible-looking wrong behaviour rather
;; than an error: a directory resolving to the wrong account, or the feature
;; standing down silently.  Nothing surfaces those but exercising them.

;;; Code:

(require 'ert)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defvar claude-account-tests--loaded nil)

(defun claude-account-tests--setup ()
  (unless claude-account-tests--loaded
    (cfg-test-require-package "claude-code" 'claude-code)
    (cfg-test-eval-init-region
     ";; Select which Claude Code account a session runs under"
     "(define-key claude-code-command-map (kbd \"A\") #'my/claude-account-show)))")
    (setq claude-account-tests--loaded t)))

(defmacro claude-account-tests--with-tree (spec &rest body)
  "Build a temporary directory tree from SPEC and run BODY with `root' bound.
SPEC is a list of (RELATIVE-PATH . ACCOUNT-OR-NIL)."
  (declare (indent 1))
  `(let ((root (make-temp-file "acct-test" t)))
     (unwind-protect
         (progn
           (dolist (entry ,spec)
             (let ((dir (expand-file-name (car entry) root)))
               (make-directory dir t)
               (when (cdr entry)
                 (with-temp-file (expand-file-name ".dir-locals.el" dir)
                   (insert (format "((nil . ((my/claude-account . %S))))" (cdr entry)))))))
           ,@body)
       (delete-directory root t))))

(ert-deftest account/declaration-applies-to-its-directory ()
  (claude-account-tests--setup)
  (claude-account-tests--with-tree '(("declared" . "team"))
    (should (equal (my/claude-account-for-directory (expand-file-name "declared" root))
                   "team"))))

(ert-deftest account/declaration-inherits-into-subdirectories ()
  (claude-account-tests--setup)
  (claude-account-tests--with-tree '(("declared" . "team") ("declared/nested" . nil))
    (should (equal (my/claude-account-for-directory
                    (expand-file-name "declared/nested" root))
                   "team"))))

(ert-deftest account/nested-declaration-overrides-its-parent ()
  (claude-account-tests--setup)
  (claude-account-tests--with-tree '(("over" . "team") ("over/sub" . "max"))
    (should (equal (my/claude-account-for-directory (expand-file-name "over/sub" root))
                   "max"))
    (should (equal (my/claude-account-for-directory (expand-file-name "over" root))
                   "team"))))

(ert-deftest account/undeclared-falls-through-to-the-default ()
  (claude-account-tests--setup)
  (claude-account-tests--with-tree '(("plain" . nil))
    (let ((my/claude-account-default "max"))
      (should (equal (my/claude-account-for-directory (expand-file-name "plain" root))
                     "max")))))

(ert-deftest account/trailing-slash-does-not-shift-lookup-to-the-parent ()
  "Regression: without a trailing slash the last component is taken for a file
name, so resolution started one directory up -- a directory's own declaration
was missed while its child's was found."
  (claude-account-tests--setup)
  (claude-account-tests--with-tree '(("declared" . "team"))
    (let ((without (expand-file-name "declared" root)))
      (should (equal (my/claude-account-for-directory without) "team"))
      (should (equal (my/claude-account-for-directory (file-name-as-directory without))
                     "team")))))

(ert-deftest account/a-prefix-of-another-name-does-not-match ()
  "~/org must not capture ~/organization."
  (claude-account-tests--setup)
  (claude-account-tests--with-tree '(("org" . "team") ("organization" . nil))
    (let ((my/claude-account-default "max"))
      (should (equal (my/claude-account-for-directory (expand-file-name "organization" root))
                     "max")))))

(ert-deftest account/value-outside-the-configured-choices-is-ignored ()
  "The account becomes a command-line argument, so a checked-out repository
must not be able to inject an arbitrary one."
  (claude-account-tests--setup)
  (claude-account-tests--with-tree '(("bogus" . "--dangerously-skip-permissions"))
    (let ((my/claude-account-default "max"))
      (should (equal (my/claude-account-for-directory (expand-file-name "bogus" root))
                     "max")))))

(ert-deftest account/safe-local-variable-predicate-is-restrictive ()
  (claude-account-tests--setup)
  (should (get 'my/claude-account 'safe-local-variable))
  (should (my/claude-account-valid-p (car my/claude-account-choices)))
  (should-not (my/claude-account-valid-p "--dangerously-skip-permissions"))
  (should-not (my/claude-account-valid-p 42)))

(provide 'claude-account-tests)
;;; claude-account-tests.el ends here
