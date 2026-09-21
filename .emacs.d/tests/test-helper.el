;;; test-helper.el --- Shared scaffolding for the config tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests load only the part of `init.el' they exercise, so the suite runs
;; under `emacs -Q' in seconds rather than bootstrapping every package.
;;
;; They read the *tangled* `init.el' rather than re-implementing anything,
;; so a test failing means the config is wrong, not that a copy drifted.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst cfg-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the tests.")

(defconst cfg-emacs-dir (expand-file-name ".." cfg-test-dir)
  "The .emacs.d directory under test.")

(defun cfg-test-file (name)
  "Return NAME inside the configuration directory."
  (expand-file-name name cfg-emacs-dir))

(defun cfg-test--straight-dir (package)
  "Return PACKAGE's straight build directory, or nil when absent."
  (let ((dir (expand-file-name (concat "straight/build/" package)
                               (expand-file-name "~/.emacs.d"))))
    (and (file-directory-p dir) dir)))

(defun cfg-test-require-package (package feature)
  "Put PACKAGE on `load-path' and require FEATURE, or skip the test.
Packages are installed by straight at runtime and are not part of this
repository, so their absence skips rather than fails.

Every build directory goes on `load-path' rather than a curated list of
dependencies: that list is a second copy of what the packages already
declare, and it goes stale silently."
  (unless (cfg-test--straight-dir package)
    (ert-skip (format "%s is not installed" package)))
  (let ((build (expand-file-name "straight/build" (expand-file-name "~/.emacs.d"))))
    (dolist (dir (directory-files build t "\\`[^.]"))
      (when (file-directory-p dir) (add-to-list 'load-path dir))))
  (require feature))

(defun cfg-test-eval-init-region (start-marker end-marker)
  "Evaluate the part of `init.el' running from START-MARKER past END-MARKER.
Loading the whole file would pull in every package; this takes only the
block under test, from the artifact that actually runs.

END-MARKER need only appear somewhere inside the last form wanted: the
region is extended to that form\='s end by reading complete expressions.
Requiring the marker to land exactly on a closing paren would make these
tests break on reindentation rather than on behaviour."
  (with-temp-buffer
    (insert-file-contents (cfg-test-file "init.el"))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (unless (search-forward start-marker nil t)
      (error "Start marker not found in init.el: %s" start-marker))
    (beginning-of-line)
    (let ((beg (point)) (target nil) (stop nil))
      (unless (search-forward end-marker nil t)
        (error "End marker not found in init.el: %s" end-marker))
      (setq target (point) stop beg)
      (goto-char beg)
      (while (< stop target)
        (forward-sexp)
        (setq stop (point)))
      (eval-region beg stop t))))

;;; Fixtures reproducing what Claude actually renders.

(defun cfg-test-composer (&optional name)
  "A Claude composer, optionally on a session called NAME.
The mode line beneath the box is what distinguishes it from a dialog."
  (concat (make-string 60 ?─) (if name (concat " " name " ") " ") "─\n"
          "❯ \n" (make-string 64 ?─) "\n"
          "  user@host ~/p (main)\n"
          "  ⏵⏵ auto mode on (shift+tab to cycle) · ← for agents\n"))

(defun cfg-test-dialog (&optional focused)
  "A bordered Claude dialog whose focused row carries the pointer glyph.
This is what must never be mistaken for a composer: the folder-trust
prompt focuses its exit option, so a stray return quits Claude."
  (concat (make-string 60 ?─) "\n"
          " Do you trust the files in this folder?\n"
          "   Yes, I trust this folder\n"
          " ❯ " (or focused "No, exit") "\n"
          (make-string 60 ?─) "\n"))

(defun cfg-test-rule (&optional label)
  "A horizontal rule, optionally carrying LABEL -- as Claude's own output has."
  (concat (make-string 40 ?─) (if label (concat " " label " ") "")
          (make-string 4 ?─) "\n"))

(defmacro cfg-test-with-buffer (name contents &rest body)
  "Run BODY with a fresh buffer NAME containing CONTENTS, then kill it.
Claude buffers left behind by one test are a real name collision for the
next, which reads as a failure of the code rather than of the fixture."
  (declare (indent 2))
  `(let ((buf (get-buffer-create ,name)))
     (unwind-protect
         (progn (with-current-buffer buf (erase-buffer) (insert ,contents))
                ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

(defmacro cfg-test-capturing-sends (&rest body)
  "Run BODY with terminal writes captured in `sends', newest last."
  (declare (indent 0))
  `(let ((sends nil))
     (cl-letf (((symbol-function 'claude-code--term-send-string)
                (lambda (_backend string) (push string sends))))
       ,@body)
     (nreverse sends)))

(provide 'test-helper)
;;; test-helper.el ends here
