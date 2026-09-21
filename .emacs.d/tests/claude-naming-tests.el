;;; claude-naming-tests.el --- Buffer and session names kept together -*- lexical-binding: t -*-

;;; Commentary:

;; Run with:
;;
;;     emacs -Q --batch -l .emacs.d/tests/claude-naming-tests.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; These load the tangled `init.el' rather than a copy, so a failure means the
;; configuration is wrong and not that a fixture drifted from it.  Only the
;; naming block is evaluated, so the suite runs in seconds without
;; bootstrapping every package.
;;
;; The negative fixtures carry the weight here.  This code decides when to type
;; into a live terminal, and every mistake it has made looked reasonable: a
;; bordered dialog taken for the composer, a line of prose taken for a session
;; name, the shell's own `-c' taken for a resume.  Each has a test below named
;; for what it protects.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst claude-naming-tests-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst claude-naming-tests-init
  (expand-file-name "../init.el" claude-naming-tests-dir))

(defvar claude-naming-tests--loaded nil)

(defun claude-naming-tests--setup ()
  "Load claude-code and the naming block from the tangled init.el.
Packages are installed by straight at runtime and are not part of this
repository, so their absence skips rather than fails."
  (unless claude-naming-tests--loaded
    (let ((build (expand-file-name "straight/build" (expand-file-name "~/.emacs.d"))))
      (unless (file-directory-p (expand-file-name "claude-code" build))
        (ert-skip "claude-code is not installed"))
      (dolist (dir (directory-files build t "\\`[^.]"))
        (when (file-directory-p dir) (add-to-list 'load-path dir))))
    (require 'claude-code)
    (with-temp-buffer
      (insert-file-contents claude-naming-tests-init)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (search-forward ";; Rename a Claude session and its Emacs buffer together")
      (beginning-of-line)
      (let ((beg (point)) (target nil) (stop nil))
        (search-forward "Claude session naming is inactive")
        (setq target (point) stop beg)
        (goto-char beg)
        ;; Read whole forms, so the region never ends mid-expression.
        (while (< stop target) (forward-sexp) (setq stop (point)))
        (eval-region beg stop t)))
    (setq claude-naming-tests--loaded t)))

;;; Fixtures reproducing what Claude actually renders.

(defun claude-naming-tests--composer (&optional name)
  "The composer, optionally on a session called NAME.
The mode line beneath the box is what no dialog draws."
  (concat (make-string 60 ?─) (if name (concat " " name " ") " ") "─\n"
          "❯ \n" (make-string 64 ?─) "\n"
          "  user@host ~/p (main)\n"
          "  ⏵⏵ auto mode on (shift+tab to cycle) · ← for agents\n"))

(defun claude-naming-tests--trust-dialog ()
  "The folder-trust prompt, whose focused row is its *exit* option."
  (concat (make-string 60 ?─) "\n"
          " Do you trust the files in this folder?\n"
          "   Yes, I trust this folder\n"
          " ❯ No, exit\n"
          (make-string 60 ?─) "\n"))

(defun claude-naming-tests--picker ()
  "The resume picker: bordered, with the pointer on the focused row."
  (concat (make-string 60 ?─) "\n"
          " Select a session to resume\n"
          " ❯ roadmap        2 hours ago\n"
          "   security-vdp   yesterday\n"
          (make-string 60 ?─) "\n"))

(defun claude-naming-tests--theme-picker ()
  (concat (make-string 60 ?─) "\n Choose a theme\n ❯ dark\n   light\n"
          (make-string 60 ?─) "\n"))

(defun claude-naming-tests--starting-banner ()
  (concat (make-string 60 ?─) "\n Session is starting…\n" (make-string 60 ?─) "\n"))

(defmacro claude-naming-tests--with-buffer (name contents &rest body)
  "Run BODY with a fresh buffer NAME holding CONTENTS, then kill it.
A Claude buffer left behind is a real name collision for the next test,
which then reads as a failure of the code rather than of the fixture."
  (declare (indent 2))
  `(let ((buf (get-buffer-create ,name)))
     (unwind-protect
         (progn (with-current-buffer buf (erase-buffer) (insert ,contents)) ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

(defmacro claude-naming-tests--sends (&rest body)
  "Run BODY capturing terminal writes, newest last."
  (declare (indent 0))
  `(let ((sends nil))
     (cl-letf (((symbol-function 'claude-code--term-send-string)
                (lambda (_backend string) (push string sends))))
       ,@body)
     (nreverse sends)))

;;; Readiness — the negative cases are the point.

(ert-deftest naming/composer-is-ready ()
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" (claude-naming-tests--composer "n")
    (should (my/claude-code--session-ready-p buf))))

(ert-deftest naming/trust-dialog-is-not-ready ()
  "The pointer glyph marks the focused row of any dialog, and this one
focuses its exit option -- an injected return would quit Claude."
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" (claude-naming-tests--trust-dialog)
    (should-not (my/claude-code--session-ready-p buf))))

(ert-deftest naming/resume-picker-is-not-ready ()
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" (claude-naming-tests--picker)
    (should-not (my/claude-code--session-ready-p buf))))

(ert-deftest naming/theme-picker-is-not-ready ()
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" (claude-naming-tests--theme-picker)
    (should-not (my/claude-code--session-ready-p buf))))

(ert-deftest naming/starting-banner-is-not-ready ()
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" (claude-naming-tests--starting-banner)
    (should-not (my/claude-code--session-ready-p buf))))

(ert-deftest naming/border-and-pointer-alone-are-not-ready ()
  "Exactly the shape of a picker, and what the previous marker matched."
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*"
      (concat (make-string 40 ?─) "\n ❯ selected row\n")
    (should-not (my/claude-code--session-ready-p buf))))


;;; Scraping the session name.

(ert-deftest naming/scrapes-the-name-above-the-composer ()
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" (claude-naming-tests--composer "roadmap")
    (should (equal (my/claude-code--scraped-session-name buf) "roadmap"))))

(ert-deftest naming/unnamed-session-scrapes-nothing ()
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" (claude-naming-tests--composer)
    (should-not (my/claude-code--scraped-session-name buf))))

(ert-deftest naming/a-rule-in-output-is-not-a-session-name ()
  "Claude prints rules of its own. They are structurally identical to a
name-bearing divider, so only position distinguishes them."
  (claude-naming-tests--setup)
  (dolist (line (list (concat (make-string 7 ?─) " Summary of changes " (make-string 7 ?─))
                      (concat (make-string 2 ?─) " ~/proj " (make-string 2 ?─)
                              " main " (make-string 2 ?─) " 12:03 " (make-string 2 ?─))
                      (concat (make-string 4 ?─) " a " (make-string 4 ?─) " b " (make-string 4 ?─))))
    (claude-naming-tests--with-buffer "*claude:p:x*"
        (concat line "\n" (claude-naming-tests--composer))
      (should-not (my/claude-code--scraped-session-name buf)))))

(ert-deftest naming/output-rules-do-not-hide-the-real-name ()
  "With prose rules above it, the composer's own divider still wins."
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*"
      (concat (make-string 7 ?─) " Summary of changes " (make-string 7 ?─) "\n"
              (claude-naming-tests--composer "roadmap"))
    (should (equal (my/claude-code--scraped-session-name buf) "roadmap"))))

(ert-deftest naming/scrapes-a-captured-live-composer ()
  "A capture from a real session, kept verbatim so a change in what Claude
renders shows up here rather than in a buffer that silently stops matching."
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*"
      (concat
       "──────────────────────────────────────────────────────── claude-code-ide ─\n"
       "❯ \n"
       "──────────────────────────────────────────────────────────────────────────\n"
       "  rod@rm-cnote ~/.dotfiles (main)                                         \n"
       "  ⏵⏵ auto mode on (shift+tab to cycle) · ← for agents\n")
    (should (my/claude-code--session-ready-p buf))
    (should (equal (my/claude-code--scraped-session-name buf) "claude-code-ide"))))

;;; Telling a resume from a fresh start.

(ert-deftest naming/resume-detection-against-real-command-lines ()
  "Claude is spawned through `sh -c \"… exec claude …\"', so a bare `-c'
anywhere in the line matched every session until detection was scoped
past the exec."
  (claude-naming-tests--setup)
  (cl-flet ((resuming (command)
              (claude-naming-tests--with-buffer "*claude:p:x*" ""
                (cl-letf (((symbol-function 'get-buffer-process) (lambda (&rest _) 'proc))
                          ((symbol-function 'process-command)
                           (lambda (&rest _) (split-string command " "))))
                  (and (my/claude-code--resuming-p buf) t)))))
    (dolist (c '("/bin/sh -c stty\\ sane\\ &&\\ exec /home/u/.local/bin/claude --resume"
                 "/bin/sh -c stty\\ sane\\ &&\\ exec claude-acct max --continue"
                 "/bin/sh -c exec claude -r"
                 "/bin/sh -c exec claude -c"
                 "/bin/sh -c exec claude --resume=abc123"))
      (should (resuming c)))
    ;; The shell's own -c must not read as a resume.
    (dolist (c '("/bin/sh -c stty\\ sane\\ &&\\ exec claude-acct max"
                 "/bin/sh -c exec claude"
                 "/bin/sh -c exec claude --resumeish"))
      (should-not (resuming c)))))

;;; Regression for the review of 881b8e7.

(ert-deftest naming/using-the-resume-picker-does-not-cancel-adoption ()
  "The picker runs in this buffer, so the keys used to choose a session are
commands. Cancelling the poll on them abandoned the adoption of the very
name just chosen, on every picker resume."
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" (claude-naming-tests--picker)
    (with-current-buffer buf
      (my/claude-code--poll-again #'ignore buf (+ (float-time) 30))
      (should my/claude-code--naming-timer)
      ;; What choosing from the picker looks like.
      (run-hooks 'pre-command-hook)
      (should my/claude-code--naming-timer)
      (cancel-timer my/claude-code--naming-timer))))

(ert-deftest naming/typing-cancels-only-on-the-create-path ()
  "There the poll ends by typing into the session, so a late escape would
interrupt a turn deliberately started."
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:x*" "not ready\n"
    (with-current-buffer buf
      (my/claude-code--poll-again #'ignore buf (+ (float-time) 30) t)
      (should my/claude-code--naming-timer)
      (run-hooks 'pre-command-hook)
      (should-not my/claude-code--naming-timer))))

(ert-deftest naming/read-back-waits-past-the-pre-send-name ()
  "The reconcile can read before the CLI redraws. Without knowing what the
divider showed beforehand, that first read is the old name and the buffer
follows itself straight back to it."
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:wanted*" (claude-naming-tests--composer "old")
    (my/claude-code--reconcile-after-rename buf "wanted" "old" (- (float-time) 1))
    (should (equal (buffer-name buf) "*claude:p:wanted*"))))

(ert-deftest naming/read-back-follows-a-yielded-name ()
  "Session names are unique per machine, so the CLI answers with a different
one when the asked-for name is held."
  (claude-naming-tests--setup)
  (claude-naming-tests--with-buffer "*claude:p:wanted*" (claude-naming-tests--composer "yielded-2")
    (my/claude-code--reconcile-after-rename buf "wanted" "old" (+ (float-time) 5))
    (should (equal (buffer-name buf) "*claude:p:yielded-2*"))))

(provide 'claude-naming-tests)
;;; claude-naming-tests.el ends here
