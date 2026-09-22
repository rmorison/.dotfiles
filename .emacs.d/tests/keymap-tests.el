;;; keymap-tests.el --- Added bindings must not displace upstream ones -*- lexical-binding: t -*-

;;; Commentary:

;; Binding a key that upstream already uses is silent: the old command simply
;; stops working, and you find out when you reach for it.  This happened here
;; once already, taking out `claude-code-send-escape'.

;;; Code:

(require 'ert)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest keymap/claude-bindings-do-not-displace-upstream ()
  (cfg-test-require-package "claude-code" 'claude-code)
  (cfg-test-eval-init-region
   ";; Select which Claude Code account a session runs under"
   "(kbd \"A\") #'my/claude-account-show")
  (cfg-test-eval-init-region
   ";; Rename a Claude session and its Emacs buffer together"
   "(kbd \"N\") #'my/claude-code-rename")
  ;; Ours landed where intended.
  (should (eq (lookup-key claude-code-command-map (kbd "a")) #'my/claude-code-with-account))
  (should (eq (lookup-key claude-code-command-map (kbd "A")) #'my/claude-account-show))
  (should (eq (lookup-key claude-code-command-map (kbd "N")) #'my/claude-code-rename))
  ;; And upstream's neighbours still work. `n' is the one this took out before.
  (should (eq (lookup-key claude-code-command-map (kbd "n")) #'claude-code-send-escape))
  (should (eq (lookup-key claude-code-command-map (kbd "r")) #'claude-code-send-region))
  (should (eq (lookup-key claude-code-command-map (kbd "R")) #'claude-code-resume))
  (should (eq (lookup-key claude-code-command-map (kbd "c")) #'claude-code)))

(ert-deftest keymap/vterm-url-binding-does-not-displace-vterm ()
  (cfg-test-require-package "vterm" 'vterm)
  (cfg-test-eval-init-region
   ";; Follow URLs printed in terminal output."
   "#'browse-url-at-point")
  ;; Bound in both maps: copy mode clears the local map, so a binding in
  ;; `vterm-mode-map' alone is undefined exactly where links are hunted.
  (should (eq (lookup-key vterm-mode-map (kbd "C-c C-o")) #'browse-url-at-point))
  (should (eq (lookup-key vterm-copy-mode-map (kbd "C-c C-o")) #'browse-url-at-point))
  (should (eq (lookup-key vterm-mode-map (kbd "C-c C-t")) #'vterm-copy-mode))
  (should (eq (lookup-key vterm-mode-map (kbd "C-c C-n")) #'vterm-next-prompt)))

(ert-deftest keymap/rebinding-warns-about-nothing ()
  "Re-evaluating the config must not report our own bindings as conflicts.
`with-eval-after-load' runs immediately once the feature is loaded, so a
reload takes this path every time."
  (cfg-test-require-package "vterm" 'vterm)
  ;; All three entry points, not just `warn'. `warn' is a wrapper around
  ;; `display-warning' in source, but it is preloaded and the call does not go
  ;; through the symbol, so stubbing `display-warning' alone captures nothing
  ;; from `warn' -- the test would pass without looking at anything. Stubbing
  ;; only `warn' is correct for what init.el writes today and silently stops
  ;; looking the day one of these becomes an `lwarn'.
  (let (warnings)
    (cl-letf (((symbol-function 'warn)
               (lambda (&rest args) (push args warnings)))
              ((symbol-function 'lwarn)
               (lambda (&rest args) (push args warnings)))
              ((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (cfg-test-eval-init-region
       ";; Follow URLs printed in terminal output."
       "#'browse-url-at-point")
      (cfg-test-eval-init-region
       ";; Follow URLs printed in terminal output."
       "#'browse-url-at-point"))
    (should-not warnings)))

(provide 'keymap-tests)
;;; keymap-tests.el ends here
