;;; vterm-url-tests.el --- URLs rejoined across terminal wraps -*- lexical-binding: t -*-

;;; Commentary:

;; Claude's renderer fills a row, lets the terminal wrap, then writes a hanging
;; indent as real text.  The failure this guards against looked like success:
;; the browser opened, on a URL cut off at the right edge of the window.
;;
;; The fixture is that shape exactly, faces included, as read out of a live
;; Claude buffer.  Copy mode leaves the fake newlines where they are -- removing
;; them corrupts the buffer under Claude, which `init.el' must never re-enable --
;; so the same layout serves for copy mode and a live terminal.

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "test-helper"
                        (file-name-directory (or load-file-name buffer-file-name))))

(defvar vterm-url-tests--loaded nil)

(defun vterm-url-tests--setup ()
  (unless vterm-url-tests--loaded
    (cfg-test-require-package "vterm" 'vterm)
    (cfg-test-eval-init-region
     ";; Rejoin URLs that Claude split across rows"
     "advice-add 'goto-address-fontify")
    (require 'goto-addr)
    (setq vterm-url-tests--loaded t)))

(defconst vterm-url-tests--url
  "https://monitoring.example.com/console/home?region=us-west-2#dashboards/dashboard/service-overview")

(defun vterm-url-tests--link (s) (propertize s 'font-lock-face '(:foreground "#6666FF")))
(defun vterm-url-tests--text (s) (propertize s 'font-lock-face '(:foreground "#556b72")))
(defun vterm-url-tests--wrap () (propertize "\n" 'vterm-line-wrap t 'rear-nonsticky t))

(defun vterm-url-tests--fixture ()
  "Claude's output as vterm holds it: two prose wraps, then a wrapped URL.
The prose wraps come first so that copy mode has removed earlier breaks
by the time it reaches the URL's -- which is what shifts its position."
  (let ((l #'vterm-url-tests--link) (x #'vterm-url-tests--text)
        (w (vterm-url-tests--wrap)))
    (concat (funcall x "  so turning it on would") w (funcall x "  put the cache in front") "\n"
            (funcall x "  read as") w (funcall x "  \"the service is down.\"") "\n"
            "\n"
            (funcall x "  Dashboard — your main view") "\n"
            (funcall x "  ")
            (funcall l "https://monitoring.example.com/console/home?region=us-west-2#dashboards/dashbo")
            w (funcall x "  ") (funcall l "ard/service-overview") "\n"
            "\n"
            (funcall x "  The status page") "\n")))

(defmacro vterm-url-tests--with (contents &rest body)
  "Run BODY in a vterm-like buffer holding CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (setq-local major-mode 'vterm-mode)
     (my/vterm-wrapped-url-setup)
     (goto-char (point-min))
     ,@body))

(defun vterm-url-tests--url-at (needle)
  "The URL `browse-url-at-point' would open with point inside NEEDLE."
  (goto-char (point-min))
  ;; Case-sensitive, or "dashbo" lands in the prose "Dashboard".
  (let ((case-fold-search nil)) (search-forward needle))
  (backward-char 2)
  (browse-url-url-at-point))

;;; Following

(ert-deftest vterm-url/first-half-opens-whole-url ()
  (vterm-url-tests--setup)
  (vterm-url-tests--with (vterm-url-tests--fixture)
    (should (equal (vterm-url-tests--url-at "dashbo") vterm-url-tests--url))))

(ert-deftest vterm-url/second-half-opens-whole-url ()
  (vterm-url-tests--setup)
  (vterm-url-tests--with (vterm-url-tests--fixture)
    (should (equal (vterm-url-tests--url-at "service-over") vterm-url-tests--url))))

(ert-deftest vterm-url/three-rows ()
  (vterm-url-tests--setup)
  (let ((l #'vterm-url-tests--link) (x #'vterm-url-tests--text)
        (w (vterm-url-tests--wrap)))
    (vterm-url-tests--with
        (concat (funcall x "  ") (funcall l "https://example.com/aaa") w
                (funcall x "  ") (funcall l "bbb") w
                (funcall x "  ") (funcall l "ccc") "\n")
      (should (equal (vterm-url-tests--url-at "bbb") "https://example.com/aaabbbccc")))))

;;; What must not be joined

(ert-deftest vterm-url/prose-after-a-full-row-url-is-not-joined ()
  "A URL that happens to end at the edge, followed by prose on the next row."
  (vterm-url-tests--setup)
  (vterm-url-tests--with
      (concat (vterm-url-tests--text "  see ")
              (vterm-url-tests--link "https://example.com/full")
              (vterm-url-tests--wrap)
              (vterm-url-tests--text "  and then more") "\n")
    (should (equal (vterm-url-tests--url-at "full") "https://example.com/full"))))

(ert-deftest vterm-url/real-newline-is-not-joined ()
  "A newline the program wrote is real, whatever follows it."
  (vterm-url-tests--setup)
  (vterm-url-tests--with
      (concat (vterm-url-tests--text "  ")
              (vterm-url-tests--link "https://example.com/a") "\n"
              (vterm-url-tests--text "  ")
              (vterm-url-tests--link "b/c") "\n")
    (should (equal (vterm-url-tests--url-at "/a") "https://example.com/a"))))

(ert-deftest vterm-url/blank-before-the-wrap-is-not-joined ()
  "The row ended in a space, so the URL ended before the wrap."
  (vterm-url-tests--setup)
  (vterm-url-tests--with
      (concat (vterm-url-tests--link "  https://example.com/a ")
              (vterm-url-tests--wrap)
              (vterm-url-tests--link "  b/c") "\n")
    (should (equal (vterm-url-tests--url-at "/a") "https://example.com/a"))))

;;; Highlighting

(defun vterm-url-tests--continuations ()
  "URL overlays on the continuation row of the fixture's wrapped URL."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)) (search-forward "service-over"))
    (seq-filter (lambda (o) (eq (overlay-get o 'category) 'goto-address))
                (overlays-at (point)))))

(defun vterm-url-tests--fontify-row (needle)
  "Fontify only the row holding NEEDLE, as one `jit-lock' chunk would."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)) (search-forward needle))
    (goto-address-fontify (line-beginning-position) (line-end-position))))

(ert-deftest vterm-url/highlights-and-follows-continuation ()
  (vterm-url-tests--setup)
  (vterm-url-tests--with (vterm-url-tests--fixture)
    (goto-address-fontify)
    (let ((ov (car (vterm-url-tests--continuations))))
      (should ov)
      (should (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                     "ard/service-overview"))
      (let (opened)
        (cl-letf (((symbol-function 'browse-url-button-open-url)
                   (lambda (url) (setq opened url))))
          (goto-address--button-action ov))
        (should (equal opened vterm-url-tests--url))))))

(ert-deftest vterm-url/continuation-survives-separate-fontification ()
  "`jit-lock' fontifying the second row clears its overlays, continuation included."
  (vterm-url-tests--setup)
  (vterm-url-tests--with (vterm-url-tests--fixture)
    (vterm-url-tests--fontify-row "dashbo")
    (vterm-url-tests--fontify-row "service-over")
    (should (= 1 (length (vterm-url-tests--continuations))))))

(ert-deftest vterm-url/refontifying-does-not-stack-continuations ()
  (vterm-url-tests--setup)
  (vterm-url-tests--with (vterm-url-tests--fixture)
    (vterm-url-tests--fontify-row "service-over")
    (vterm-url-tests--fontify-row "dashbo")
    (vterm-url-tests--fontify-row "dashbo")
    (should (= 1 (length (vterm-url-tests--continuations))))))

(ert-deftest vterm-url/highlighting-outside-vterm-is-untouched ()
  "The advice is global; it must do nothing in any other buffer."
  (vterm-url-tests--setup)
  (with-temp-buffer
    (insert (concat (vterm-url-tests--link "https://example.com/a")
                    (vterm-url-tests--wrap)
                    (vterm-url-tests--link "bcd") "\n"))
    (goto-address-fontify)
    (should-not (cl-some (lambda (o) (overlay-get o 'goto-address))
                         (overlays-at (- (point-max) 2))))))

;;; The setting that corrupted Claude buffers

(defun vterm-url-tests--sets-non-nil (form symbol)
  "Non-nil if FORM anywhere pairs SYMBOL with a non-nil value.
Covers `setq', `setopt', `customize-set-variable' and `use-package'
`:custom' entries alike, since each puts the value right after the name."
  (and (consp form)
       (or (let ((tail form) found)
             (while (and (consp tail) (not found))
               (setq found (and (eq (car tail) symbol)
                                (consp (cdr tail))
                                (cadr tail)
                                (not (equal (cadr tail) ''nil))))
               (setq tail (cdr tail)))
             found)
           (let ((tail form) found)
             (while (and (consp tail) (not found))
               (setq found (vterm-url-tests--sets-non-nil (car tail) symbol)
                     tail (cdr tail)))
             found))))

(ert-deftest vterm-url/init-never-removes-fake-newlines-in-copy-mode ()
  "Claude keeps redrawing during copy mode -- its pty is -ixon, so vterm's
XOFF arrives as a keypress -- and removing the fake newlines then leaves
the buffer short of the lines libvterm expects.  From then on `vterm-yank'
and every other cursor sync fails with \"End of buffer\"."
  (with-temp-buffer
    (insert-file-contents (cfg-test-file "init.el"))
    (goto-char (point-min))
    (let (form offenders)
      (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
        (when (vterm-url-tests--sets-non-nil form 'vterm-copy-mode-remove-fake-newlines)
          (push form offenders)))
      (should-not offenders))))

(provide 'vterm-url-tests)
;;; vterm-url-tests.el ends here
