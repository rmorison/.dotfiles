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
  "Re-tangling Emacs.org must reproduce the committed init.el byte for byte."
  (let* ((org (cfg-test-file "Emacs.org"))
         (committed (with-temp-buffer
                      (insert-file-contents (cfg-test-file "init.el"))
                      (buffer-string)))
         (scratch (make-temp-file "tangle-test" t))
         (org-copy (expand-file-name "Emacs.org" scratch)))
    (unwind-protect
        (progn
          (copy-file org org-copy t)
          ;; Tangle the copy so a stale or dirty init.el is never written over.
          (let ((org-babel-default-header-args
                 (cons '(:tangle . "./init.el") org-babel-default-header-args))
                (org-confirm-babel-evaluate nil))
            (org-babel-tangle-file org-copy))
          (let ((fresh (with-temp-buffer
                         (insert-file-contents (expand-file-name "init.el" scratch))
                         (buffer-string))))
            (should (equal committed fresh))))
      (delete-directory scratch t))))

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
