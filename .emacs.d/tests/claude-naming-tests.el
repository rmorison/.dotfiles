;;; claude-naming-tests.el --- Buffer and session names kept together -*- lexical-binding: t -*-

;;; Commentary:

;; The negative fixtures carry the weight here.  This code decides when to type
;; into a live terminal, and every mistake it has made looked reasonable: a
;; bordered dialog taken for the composer, a line of prose taken for a session
;; name, the shell's own `-c' taken for a resume.  Each has a test below named
;; for what it protects.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defvar claude-naming-tests--loaded nil)

(defun claude-naming-tests--setup ()
  (unless claude-naming-tests--loaded
    (cfg-test-require-package "claude-code" 'claude-code)
    (cfg-test-eval-init-region
     ";; Rename a Claude session and its Emacs buffer together"
     "Claude session naming is inactive")
    (setq claude-naming-tests--loaded t)))

;;; Readiness — the negative cases are the point.

(ert-deftest naming/composer-is-ready ()
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*" (cfg-test-composer "n")
    (should (my/claude-code--session-ready-p buf))))

(ert-deftest naming/trust-dialog-is-not-ready ()
  "The pointer glyph marks the focused row of any dialog, and this one
focuses its exit option -- an injected return would quit Claude."
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*" (cfg-test-dialog "Do you trust the files in this folder?"
                       '("Yes, I trust this folder" "No, exit") "No, exit")
    (should-not (my/claude-code--session-ready-p buf))))

(ert-deftest naming/resume-picker-is-not-ready ()
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*" (cfg-test-dialog "Select a session to resume"
                       '("roadmap        2 hours ago" "security-vdp   yesterday"))
    (should-not (my/claude-code--session-ready-p buf))))

(ert-deftest naming/theme-picker-is-not-ready ()
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*" (cfg-test-dialog "Choose a theme" '("dark" "light"))
    (should-not (my/claude-code--session-ready-p buf))))

(ert-deftest naming/starting-banner-is-not-ready ()
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*" (cfg-test-dialog "Session is starting…")
    (should-not (my/claude-code--session-ready-p buf))))

(ert-deftest naming/border-and-pointer-alone-are-not-ready ()
  "Exactly the shape of a picker, and what the previous marker matched."
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*"
      (concat (make-string 40 ?─) "\n ❯ selected row\n")
    (should-not (my/claude-code--session-ready-p buf))))


;;; Scraping the session name.

(ert-deftest naming/scrapes-the-name-above-the-composer ()
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*" (cfg-test-composer "roadmap")
    (should (equal (my/claude-code--scraped-session-name buf) "roadmap"))))

(ert-deftest naming/unnamed-session-scrapes-nothing ()
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*" (cfg-test-composer)
    (should-not (my/claude-code--scraped-session-name buf))))

(ert-deftest naming/a-rule-in-output-is-not-a-session-name ()
  "Claude prints rules of its own. They are structurally identical to a
name-bearing divider, so only position distinguishes them."
  (claude-naming-tests--setup)
  (dolist (line (list (concat (make-string 7 ?─) " Summary of changes " (make-string 7 ?─))
                      (concat (make-string 2 ?─) " ~/proj " (make-string 2 ?─)
                              " main " (make-string 2 ?─) " 12:03 " (make-string 2 ?─))
                      (concat (make-string 4 ?─) " a " (make-string 4 ?─) " b " (make-string 4 ?─))))
    (cfg-test-with-buffer "*claude:p:x*"
        (concat line "\n" (cfg-test-composer))
      (should-not (my/claude-code--scraped-session-name buf)))))

(ert-deftest naming/output-rules-do-not-hide-the-real-name ()
  "With prose rules above it, the composer's own divider still wins."
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*"
      (concat (make-string 7 ?─) " Summary of changes " (make-string 7 ?─) "\n"
              (cfg-test-composer "roadmap"))
    (should (equal (my/claude-code--scraped-session-name buf) "roadmap"))))

(ert-deftest naming/scrapes-a-captured-live-composer ()
  "A capture from a real session, kept verbatim so a change in what Claude
renders shows up here rather than in a buffer that silently stops matching."
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:x*"
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
              (cfg-test-with-buffer "*claude:p:x*" ""
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
  (cfg-test-with-buffer "*claude:p:x*" (cfg-test-dialog "Select a session to resume"
                       '("roadmap        2 hours ago" "security-vdp   yesterday"))
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
  (cfg-test-with-buffer "*claude:p:x*" "not ready\n"
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
  (cfg-test-with-buffer "*claude:p:wanted*" (cfg-test-composer "old")
    (my/claude-code--reconcile-after-rename buf "wanted" "old" (- (float-time) 1))
    (should (equal (buffer-name buf) "*claude:p:wanted*"))))

(ert-deftest naming/read-back-follows-a-yielded-name ()
  "Session names are unique per machine, so the CLI answers with a different
one when the asked-for name is held."
  (claude-naming-tests--setup)
  (cfg-test-with-buffer "*claude:p:wanted*" (cfg-test-composer "yielded-2")
    (my/claude-code--reconcile-after-rename buf "wanted" "old" (+ (float-time) 5))
    (should (equal (buffer-name buf) "*claude:p:yielded-2*"))))

(provide 'claude-naming-tests)
;;; claude-naming-tests.el ends here
