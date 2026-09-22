;;; tangle-tests.el --- init.el must match Emacs.org -*- lexical-binding: t -*-

;;; Commentary:

;; The characteristic failure of a literate configuration is a tangled
;; `init.el' committed out of step with its source.  Nothing else notices:
;; the file is valid, it loads, and it quietly runs code the org file no
;; longer describes.  These checks need no knowledge of any block.

;;; Code:

(require 'ert)
(require 'org)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest tangle/init-el-is-current ()
  "Re-tangling Emacs.org must reproduce the committed init.el.
Compared ignoring trailing whitespace: org versions disagree about
whether a blank line inside an indented block keeps its indentation, so
a byte-exact check asserts which org tangled the file rather than
whether the file still corresponds to its source."
  (let* ((org (cfg-test-file "Emacs.org"))
         (committed (cfg-test-normalise-tangle
                     (with-temp-buffer
                       (insert-file-contents (cfg-test-file "init.el"))
                       (buffer-string))))
         (scratch (make-temp-file "tangle-test" t))
         (org-copy (expand-file-name "Emacs.org" scratch)))
    (unwind-protect
        (progn
          (copy-file org org-copy t)
          ;; Tangle the copy so a stale or dirty init.el is never written over.
          ;; Where the blocks go is Emacs.org's own business -- its
          ;; `#+PROPERTY' line, asserted by the test below -- and the relative
          ;; path in it lands beside whichever copy is being tangled.
          (let ((org-confirm-babel-evaluate nil))
            (org-babel-tangle-file org-copy))
          (let ((fresh (cfg-test-normalise-tangle
                        (with-temp-buffer
                          (insert-file-contents (expand-file-name "init.el" scratch))
                          (buffer-string)))))
            ;; Report where they part rather than dumping two whole files:
            ;; ERT prints both on failure, which is thousands of lines and
            ;; gets truncated by CI log viewers exactly when it is needed.
            (unless (equal committed fresh)
              (ert-fail (list "init.el does not match a fresh tangle of Emacs.org"
                              :org-version (org-version)
                              :emacs-version emacs-version
                              :first-difference
                              (cfg-test-describe-difference committed fresh))))))
      (delete-directory scratch t))))

(ert-deftest tangle/org-file-directs-tangling ()
  "Emacs.org must name init.el as the tangle target for its elisp blocks.
Lose that header and tangling produces nothing: `make tangle' reports
success, init.el is left untouched at whatever it already was, and the
configuration silently stops tracking its source.  Only `emacs-lisp'
blocks are directed there -- the shell blocks are documentation, and a
header broad enough to catch them would tangle them into init.el too."
  (with-temp-buffer
    (insert-file-contents (cfg-test-file "Emacs.org"))
    (goto-char (point-min))
    (should (re-search-forward
             "^#\\+PROPERTY: +header-args:emacs-lisp +:tangle +\\./init\\.el *$"
             nil t))))

(ert-deftest tangle/init-el-parens-balance ()
  "An unbalanced init.el fails at startup, after some of it has run."
  (with-temp-buffer
    (insert-file-contents (cfg-test-file "init.el"))
    (emacs-lisp-mode)
    (should (progn (check-parens) t))))

(ert-deftest tangle/init-el-declares-lexical-binding ()
  "The cookie only counts on the first line, so the header block must stay first."
  (with-temp-buffer
    (insert-file-contents (cfg-test-file "init.el") nil 0 200)
    (goto-char (point-min))
    (should (re-search-forward "lexical-binding:[ ]*t" (line-end-position) t))))

(provide 'tangle-tests)
;;; tangle-tests.el ends here
