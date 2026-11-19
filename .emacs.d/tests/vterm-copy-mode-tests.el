;;; vterm-copy-mode-tests.el --- Leaving copy mode never strands the buffer -*- lexical-binding: t -*-

;;; Commentary:

;; `vterm--exit-copy-mode' moves point to the terminal cursor before it
;; restores the keymap.  When that move signals `end-of-buffer', the buffer
;; was left with no local keymap, and every key reported "Buffer is
;; read-only" -- which looks like a stuck read-only mode, not a lost cursor.
;;
;; There is no terminal here: the buffer only claims to be in `vterm-mode',
;; and the cursor lookup and flow-control sends are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defvar vterm-copy-mode-tests--loaded nil)

(defun vterm-copy-mode-tests--setup ()
  (unless vterm-copy-mode-tests--loaded
    (cfg-test-require-package "vterm" 'vterm)
    (cfg-test-eval-init-region
     ";; Restore the terminal keymap when leaving copy mode fails"
     "advice-add 'vterm--exit-copy-mode")
    (setq vterm-copy-mode-tests--loaded t)))

(defmacro vterm-copy-mode-tests--with-buffer (cursor-fn &rest body)
  "Run BODY in a buffer posing as vterm, with CURSOR-FN finding the cursor.
Warnings are collected in `warnings' instead of shown."
  (declare (indent 1))
  `(progn
     (vterm-copy-mode-tests--setup)
     (let ((warnings nil))
       (with-temp-buffer
         (setq major-mode 'vterm-mode)
         (use-local-map vterm-mode-map)
         (cl-letf (((symbol-function 'vterm-reset-cursor-point) ,cursor-fn)
                   ((symbol-function 'vterm-send-stop) #'ignore)
                   ((symbol-function 'vterm-send-start) #'ignore)
                   ((symbol-function 'display-warning)
                    (lambda (_type msg &rest _) (push msg warnings))))
           ,@body)))))

(ert-deftest vterm-copy-mode/lost-cursor-restores-keymap ()
  (vterm-copy-mode-tests--with-buffer
      (lambda () (signal 'end-of-buffer nil))
    (vterm-copy-mode 1)
    (should (null (current-local-map)))
    (vterm-copy-mode -1)
    (should-not vterm-copy-mode)
    (should (eq (current-local-map) vterm-mode-map))
    (should (eq (lookup-key (current-local-map) "a") 'vterm--self-insert))
    (should (= (length warnings) 1))
    (should (string-match-p "lost the terminal cursor" (car warnings)))))

(ert-deftest vterm-copy-mode/lost-cursor-still-runs-mode-hooks ()
  ;; claude-code.el rebuilds its keymap from this hook, parented to whatever
  ;; the local map is.  It must run, and see `vterm-mode-map' when it does.
  (vterm-copy-mode-tests--with-buffer
      (lambda () (signal 'end-of-buffer nil))
    (let ((seen 'not-run))
      (add-hook 'vterm-copy-mode-hook
                (lambda () (unless vterm-copy-mode (setq seen (current-local-map))))
                nil t)
      (vterm-copy-mode 1)
      (vterm-copy-mode -1)
      (should (eq seen vterm-mode-map)))))

(ert-deftest vterm-copy-mode/normal-exit-is-untouched ()
  (vterm-copy-mode-tests--with-buffer #'ignore
    (vterm-copy-mode 1)
    (vterm-copy-mode -1)
    (should (eq (current-local-map) vterm-mode-map))
    (should (null warnings))))

;;; vterm-copy-mode-tests.el ends here
