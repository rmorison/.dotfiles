;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/.emacs.d/Emacs.org"))
    ;; Dyname scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defvar bootstrap-version)
(let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t) ;; Automatically use `straight.el` with `use-package`


(use-package no-littering)

;; Functions for upgrading packages
(defun efs/upgrade-all-packages ()
  "Upgrade all straight.el packages."
  (interactive)
  (message "Upgrading all packages...")
  (straight-pull-all)
  (straight-rebuild-all)
  (message "All packages upgraded!"))

(defun efs/upgrade-package (package)
  "Upgrade a specific PACKAGE."
  (interactive
   (list (completing-read "Upgrade package: "
                         (hash-table-keys straight--recipe-cache)
                         nil t)))
  (message "Upgrading %s..." package)
  (straight-pull-package package)
  (straight-rebuild-package package)
  (message "Package %s upgraded!" package))

;; Add these to global key bindings
(global-set-key (kbd "C-c u a") 'efs/upgrade-all-packages)
(global-set-key (kbd "C-c u p") 'efs/upgrade-package)

;; You will most likely need to adjust this font size for your system!
;; If fonts are missing
;; sudo apt install fonts-firacode fonts-cantarell
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)
(defvar efs/default-font-family "Fira Code")
(defvar efs/fixed-font-family "Fira Code")
(defvar efs/variable-font-family "Cantarell")
(defvar efs/default-fill-column 112)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; MacOS key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (message "adding %s inits" (system-name))

  ;; osx may require the following...
  ;;
  ;; brew install svn
  ;; brew tap homebrew/cask-fonts
  ;; brew install --cask font-fira-code font-fira-mono
  ;; brew install --cask font-cantarell
  ;; brew install coreutils

  ;; these mac-* settings assumes System->Keyboard->Modifier Keys...
  ;; Caps Lock Key: Control
  ;; Control Key  : Option
  ;; Option Key   : Command
  ;; Command Key  : Command
  (setq mac-control-modifier 'super)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq insert-directory-program "gls" dired-use-ls-dired t)

  ;; nice up the osx screen on 3440x1440 display
  (setq efs/default-font-size 160)
  (setq efs/default-variable-font-size 160))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 500 1000 1000))
(setq read-process-output-max (* 2 1024 1024))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; top level look/feel
(setq inhibit-startup-message t)

(if window-system
    (progn
      (set-fringe-mode 10)        ; Give some breathing room
      (scroll-bar-mode -1)))        ; Disable visible scrollbar

(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(display-time-mode 1)       ; Anyone know what time it is?
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Maximum frame because we love Emacs
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; line and column numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                eat-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                dired-mode-hook
                org-agenda-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; default face
(set-face-attribute 'default nil
                    :font efs/default-font-family
                    :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font efs/fixed-font-family
                    :height efs/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font efs/variable-font-family
                    :height efs/default-variable-font-size
                    :weight 'regular)

(global-auto-revert-mode 1)

;;(load-theme 'tango-dark)
;; have tried: doom-palenight doom-material-dark doom-solarized-light doom-solarized-light doom-zenburn doom-monokai-machine doom-oceanic-next
(use-package doom-themes
  :init (load-theme 'doom-solarized-light t)
  :config
  (custom-set-faces
   '(region ((t (:background "#cfcbc6"))))))

;; Replace the all-the-icons package with nerd-icons
;; all-the-icons is broken in doomemacs, see
;; https://github.com/doomemacs/doomemacs/issues/7379
;;
;; You may need to
;; M-x nerd-icons-install-fonts
;; and
;; fc-cache -f -v # from shell
(use-package nerd-icons
  :if (display-graphic-p)
  :commands nerd-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 15)
   (doom-modeline-icon t)
   (doom-modeline-major-mode-icon t)
   (doom-modeline-major-mode-color-icon t)))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; from https://www.fettesps.com/emacs-disable-suspend-button/
;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Windows Style Undo
(global-set-key [(control z)] 'undo)

;; Comment toggle
(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)

(use-package general)

(use-package which-key
  :defer t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	   ("C-c i" . imenu)
	   :map ivy-minibuffer-map
	   ("TAB" . ivy-alt-done)
	   ("C-l" . ivy-partial)
	   :map ivy-switch-buffer-map
	   ("C-l" . ivy-partial)
	   ("C-d" . ivy-switch-buffer-kill)
	   :map ivy-reverse-i-search-map
	   ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	   :map minibuffer-local-map
	   ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  ;; Configure ripgrep path
  (counsel-rg-base-command
   '("/opt/homebrew/bin/rg" "--no-heading" "--line-number" "--color" "never" "%s"))
  :config
  (counsel-mode 1)
  ;; Configure counsel-rg to start searching after 3 characters
  (setq ivy-more-chars-alist '((counsel-rg . 3))))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :defer t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

;; Python/Eglot Navigation Hydra
(defhydra hydra-python (:color blue :hint nil)
  "
^Navigation^        ^Code Actions^       ^Diagnostics^
^^^^^^^^------------------------------------------------------
_d_: definition     _r_: rename          _e_: list errors
_D_: declaration    _f_: format buffer   _n_: next error
_R_: references     _F_: format region   _p_: prev error
_i_: implementation _a_: code actions    _q_: quit
_t_: type def       _h_: hover
"
  ("d" xref-find-definitions)
  ("D" eglot-find-declaration)
  ("R" xref-find-references)
  ("i" eglot-find-implementation)
  ("t" eglot-find-typeDefinition)
  ("r" eglot-rename)
  ("f" eglot-format-buffer)
  ("F" eglot-format)
  ("a" eglot-code-actions)
  ("h" eldoc-doc-buffer)
  ("e" flymake-show-diagnostics-buffer)
  ("n" flymake-goto-next-error)
  ("p" flymake-goto-prev-error)
  ("q" nil))

;; Window Management Hydra
(defhydra hydra-window (:color red :hint nil)
  "
^Move^          ^Split^         ^Resize^          ^Other^
^^^^^^^^--------------------------------------------------------
_h_: left       _v_: vertical   _H_: shrink h     _d_: delete
_j_: down       _x_: horizontal _J_: enlarge v    _D_: delete other
_k_: up         _z_: undo       _K_: shrink v     _b_: balance
_l_: right      _Z_: redo       _L_: enlarge h    _f_: new frame
_o_: other                                        _q_: quit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("o" other-window)
  ("v" split-window-right)
  ("x" split-window-below)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("b" balance-windows)
  ("f" make-frame-command)
  ("H" shrink-window-horizontally)
  ("J" enlarge-window)
  ("K" shrink-window)
  ("L" enlarge-window-horizontally)
  ("z" winner-undo)
  ("Z" winner-redo)
  ("q" nil))

;; Text Scaling Hydra
(defhydra hydra-zoom (:color red :hint nil)
  "
Zoom: _+_/_=_ in, _-_ out, _0_ reset, _q_ quit
"
  ("+" text-scale-increase)
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-set 0))
  ("q" nil))

;; Git Operations Hydra (for Magit)
(defhydra hydra-git (:color blue :hint nil)
  "
^Status^      ^Changes^        ^History^       ^Actions^
^^^^^^^^--------------------------------------------------------
_s_: status   _d_: diff        _l_: log        _c_: commit
_b_: blame    _D_: diff dwim   _L_: log file   _p_: push
_t_: timemachine               _r_: reflog     _P_: pull
_f_: file dispatch             _w_: worktree   _!_: git command
                                               _q_: quit
"
  ("s" magit-status)
  ("b" magit-blame)
  ("d" magit-diff)
  ("D" magit-diff-dwim)
  ("l" magit-log)
  ("L" magit-log-buffer-file)
  ("r" magit-reflog)
  ("t" git-timemachine)
  ("f" magit-file-dispatch)
  ("w" magit-worktree)
  ("c" magit-commit)
  ("p" magit-push)
  ("P" magit-pull)
  ("!" magit-git-command)
  ("q" nil))

;; Multiple Cursors Hydra
(defhydra hydra-multiple-cursors (:color red :hint nil)
  "
^Mark^            ^Edit^           ^Other^
^^^^^^^^-----------------------------------------
_n_: next         _l_: lines       _q_: quit
_p_: previous     _a_: all         _Q_: quit & disable
_N_: skip next    _r_: regexp
_P_: skip prev    _d_: defun
"
  ("n" mc/mark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("N" mc/skip-to-next-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("l" mc/edit-lines)
  ("a" mc/mark-all-like-this)
  ("r" mc/mark-all-in-region-regexp)
  ("d" mc/mark-all-like-this-in-defun)
  ("q" nil)
  ("Q" mc/keyboard-quit))

;; Global key bindings for hydras
(global-set-key (kbd "C-c h p") 'hydra-python/body)
(global-set-key (kbd "C-c h w") 'hydra-window/body)
(global-set-key (kbd "C-c h z") 'hydra-zoom/body)
(global-set-key (kbd "C-c h g") 'hydra-git/body)
(global-set-key (kbd "C-c h m") 'hydra-multiple-cursors/body)

;; Alternative bindings for quick access
(global-set-key (kbd "C-c w") 'hydra-window/body)
(global-set-key (kbd "C-c z") 'hydra-zoom/body)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))
;; eshell
(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         100000
        eshell-buffer-maximum-lines 100000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :bind (("C-r" . 'counsel-esh-history))
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies nil)
    (setq eshell-visual-commands '("htop"
                                   "zsh"
                                   "vim"
                                   "ntl"
                                   "netlify"
                                   "python"
                                   "ipython"
                                   "psql"
                                   "ssh"
                                   "mysql"
                                   "poetry"
                                   "docker"
                                   "ansible-playbook"
                                   "hugo"
                                   "aws"
                                   "copilot")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :straight (:type built-in)  ;; Tell straight.el this is a built-in package
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-listing-switches "-aghoL --group-directories-first"))

;; Load server explicitly before dirvish
(require 'server)

(use-package dirvish
  :straight (dirvish :type git :host github :repo "alexluigit/dirvish")
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("p" "~/Projects/"                 "Projects")
     ("s" "~/Screenshots/"              "Screenshots")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state))
  (setq delete-by-moving-to-trash nil)
  (setq dirvish-hide-details nil)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

;; Install and configure eat
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; Compile terminfo
(with-eval-after-load 'eat
  (eat-compile-terminfo))

;; Basic configuration
(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

;; Enable directory tracking
(setq eat-enable-directory-tracking t)

;; Terminal settings
(setq eat-default-shell (getenv "SHELL"))
(setq eat-enable-mouse t)
(setq eat-kill-buffer-on-exit t)

;; Keybindings
(global-set-key (kbd "C-c t") #'eat)

;; Project-specific eat launcher
(defun efs/eat-project ()
  "Open eat terminal in the current project root directory."
  (interactive)
  (let ((default-directory (or (project-root (project-current))
                              default-directory)))
    (eat)))

(global-set-key (kbd "C-c T") #'efs/eat-project)

;; Vterm - Fully-featured terminal emulator
(use-package vterm
  :commands vterm
  :custom
  ;; Terminal behavior
  (vterm-max-scrollback 100000)
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback-when-clearing t)

  ;; Shell settings
  (vterm-shell (getenv "SHELL"))

  ;; Display settings
  (vterm-term-environment-variable "xterm-256color")

  ;; Performance tuning
  (vterm-timer-delay 0.01)

  ;; Disable automatic shell configuration to prevent prompt issues
  (vterm-environment '("INSIDE_EMACS=vterm"))

  :config
  ;; Set up vterm buffer display
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-reuse-window display-buffer-same-window)))

  ;; Project-specific vterm launcher
  (defun efs/vterm-project ()
    "Open vterm in the current project root directory."
    (interactive)
    (if-let* ((project (project-current))
              (root (project-root project))
              (project-name (project-name project))
              (buffer-name (format "*%s-vterm*" project-name)))
        (if (get-buffer buffer-name)
            (pop-to-buffer buffer-name)
          (let ((default-directory root))
            (vterm buffer-name)))
      (user-error "Not in a project")))

  ;; Helper function to send text to vterm
  (defun efs/vterm-send-string (string)
    "Send STRING to current vterm buffer."
    (interactive "sText to send: ")
    (vterm-send-string string))

  ;; Helper to clear scrollback
  (defun efs/vterm-clear-scrollback ()
    "Clear vterm buffer and scrollback."
    (interactive)
    (vterm-clear)
    (vterm-clear-scrollback))

  :bind
  (("C-c v" . vterm)
   ("C-c V" . efs/vterm-project)
   :map vterm-mode-map
   ("C-c C-l" . efs/vterm-clear-scrollback)
   ("C-q" . vterm-send-next-key)
   ;; Make sure these don't interfere with terminal programs
   ("M-1" . nil)
   ("M-2" . nil)
   ("M-3" . nil)
   ("M-4" . nil)
   ("M-5" . nil)
   ("M-6" . nil)
   ("M-7" . nil)
   ("M-8" . nil)
   ("M-9" . nil)
   ("M-0" . nil)))

;; Optional: vterm-toggle for quick terminal access
(use-package vterm-toggle
  :after vterm
  :bind
  (("C-c `" . vterm-toggle)
   ("C-c ~" . vterm-toggle-cd)
   :map vterm-mode-map
   ("s-n" . vterm-toggle-forward)
   ("s-p" . vterm-toggle-backward))
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-reset-window-configration-after-exit t))

;; org mode
(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :straight (:type built-in)
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq-default fill-column efs/default-fill-column)
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-image-actual-width (list 640))

  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org"))
  (when (file-exists-p "~/Blogs/rmorison.github.io/org")
    (add-to-list 'org-agenda-files "~/Blogs/rmorison.github.io/org"))

  (setq org-agenda-compact-blocks t)

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!)" "|" "DONE(d!)" "WONT-DO(w@)" "DELEGATED(D@)" "HELD-BLOCKED(h@/!)" )
          (sequence "BREAKDOWN(b)" "READY(r)" "ACTIVE(a!)" "|" "DONE(d!)" "WONT-DO(w@)" "WATCHING(W@)" "HELD-BLOCKED(h@/!)")))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "orange" :weight bold)
                ("BREAKDOWN" :foreground "dark orange" :weight bold)
                ("NEXT" :foreground "aqua" :weight bold)
                ("READY" :foreground "aqua" :weight bold)
                ("IN-PROGRESS" :foreground "forest green" :weight bold)
                ("ACTIVE" :foreground "green" :weight bold)
                ("HELD-BLOCKED" :foreground "red" :weight bold)
                ("DELEGATED" :foreground "purple" :weight bold)
                ("WATCHING" :foreground "purple" :weight bold)
                ("DONE" :foreground "white" :weight bold)
                ("WONT-DO" :foreground "grey" :weight bold))))

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("project" . ?p)
          ("agenda" . ?a)
          ("meeting" . ?m)
          ("reference" . ?n)
          ("idea" . ?i)
          ("research" . ?r)
          ("goal" . ?g)))
  (setq org-fast-tag-selection-single-key t)

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "IN-PROGRESS" ((org-agenda-overriding-header "Tasks working on now")))
            (todo "ACTIVE" ((org-agenda-overriding-header "Projects that are active")))
            (todo "NEXT" ((org-agenda-overriding-header "Tasks next up")))
            (todo "DELEGATED" ((org-agenda-overriding-header "Tasks that are delegated")))
            (todo "WATCHING" ((org-agenda-overriding-header "Projects that I'm watching")))
            (todo "HELD-BLOCKED" ((org-agenda-overriding-header "Blocked projects and tasks")))))

          ("b" "Task backlog & project planning triage"
           ((todo "TODO" ((org-agenda-overriding-header "Task backlog")))
            (todo "BREAKDOWN" ((org-agenda-overriding-header "Projects that need planning")))))

          ("c" "Completed, planned, and wont-do tasks and projects"
           ((todo "DONE"
                  ((org-agenda-overriding-header "Tasks done"))))
           ((todo "WONT-DO"
                  ((org-agenda-overriding-header "Tasks optioned to the minors")))))))

  ;; Agenda sort
  (setq org-agenda-sorting-strategy
        '((agenda habit-down todo-state-down time-up priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))

  ;; Define capture templates
  (setq org-capture-templates
        `(("t" "Task" entry (file+headline "inbox.org" "Tasks")
           (file "templates/task.org"))

          ("h" "Habit" entry (file "habits.org")
           (file "templates/habit.org"))

          ("p" "Project" entry (file+headline "projects.org" "New Projects")
           (file "templates/project.org"))

          ("n" "Note" entry (file+headline "reference.org" "Notes")
           (file "templates/note.org"))

          ("N" "Private note" entry (file "private.org")
           (file "templates/note.org"))

          ("j" "Journal" entry (file+olp+datetree "journal.org")
           (file "templates/journal.org")
           :tree-type week)

          ("m" "Meeting" entry (file+olp+datetree "meetings.org")
           (file "templates/meeting.org")
           :tree-type week)

          ("1" "1-1 Meeting" entry (file+olp+datetree "meetings.org")
           (file "templates/1-1_meeting.org")
           :tree-type week)))

  (efs/org-font-setup))

;; org mode code blocks
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("ya" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("ty" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql"))
  (add-to-list 'org-structure-template-alist '("mm" . "src mermaid"))

  ;; don't ask on eval block C-c C-c
  (setq org-confirm-babel-evaluate nil))

;; org mode key bindings
(define-key global-map (kbd "C-c c")
  (lambda () (interactive) (org-capture nil)))
;;(global-set-key (kbd "\C-cc") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "↪" "→" "○" "●" "✸" "✿" "•" "★" "•" "★" "•" "★")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width efs/default-fill-column
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package ob-go
  :defer t
  :after org)

(use-package ob-mermaid
  :defer t
  :after org
  :config
  (setq ob-mermaid-cli-path (expand-file-name "~/.nvm/versions/node/v18.16.0/bin/mmdc")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (mermaid . t)
   (shell . t)
   (python . t)
   (go . t)))

;; YAML Mode Configuration
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :hook (yaml-mode . (lambda ()
                      (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Markdown Mode for composing, editing, and reviewing markdown documents
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")  ;; Use pandoc for previewing
  :custom
  (markdown-fontify-code-blocks-natively t)  ;; Syntax highlight code blocks
  (markdown-enable-math t)  ;; Enable LaTeX math support
  (markdown-enable-wiki-links t)  ;; Enable wiki-style links
  (markdown-italic-underscore t)  ;; Use underscores for italic
  (markdown-asymmetric-header t)  ;; Don't add trailing # on headers
  (markdown-gfm-additional-languages '("shell" "bash" "python" "sql" "go" "typescript"))
  (markdown-header-scaling t)  ;; Scale headers
  (markdown-header-scaling-values '(1.5 1.3 1.1 1.0 1.0 1.0))  ;; Header scaling factors
  (markdown-hide-urls nil)  ;; Show URLs
  (markdown-indent-on-enter t)  ;; Automatically indent new lines
  (markdown-make-gfm-checkboxes-buttons t)  ;; Make checkboxes clickable
  :config
  ;; Use visual-line-mode and visual-fill-column-mode for better text wrapping
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook (lambda ()
                                  (setq visual-fill-column-width efs/default-fill-column)
                                  (visual-fill-column-mode 1)))

  ;; Key bindings
  :bind (:map markdown-mode-map
         ("C-c C-s a" . markdown-table-align)  ;; Align tables
         ("C-c C-s t" . markdown-toc-generate-toc)  ;; Generate TOC
         ("C-c C-s p" . markdown-live-preview-mode)  ;; Toggle preview
         ("C-c C-s m" . markdown-toggle-markup-hiding)  ;; Toggle markup hiding
         ("C-c C-x i" . markdown-insert-image)))  ;; Insert image

;; Live preview of Markdown
(use-package markdown-preview-mode
  :after markdown-mode
  :custom
  (markdown-preview-stylesheets
   '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.2.0/github-markdown.min.css"))
  :config
  (add-to-list 'markdown-preview-javascript
               "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js"))

;; Add markdown table of contents support
(use-package markdown-toc
  :after markdown-mode)

;; Imenu integration for markdown headers
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq imenu-generic-expression
                  '(("Heading 1" "^# \\(.+\\)" 1)
                    ("Heading 2" "^## \\(.+\\)" 1)
                    ("Heading 3" "^### \\(.+\\)" 1)
                    ("Heading 4" "^#### \\(.+\\)" 1)
                    ("Heading 5" "^##### \\(.+\\)" 1)
                    ("Heading 6" "^###### \\(.+\\)" 1)))))

;; Enable flyspell for spell checking in markdown documents
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Integrate with markdownlint if available
(when (executable-find "markdownlint")
  (use-package flymake-markdownlint
    :after markdown-mode
    :hook (markdown-mode . flymake-markdownlint-setup)))

;; Magit configuration
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (git-commit-summary-max-length 88)
  (git-commit-fill-column 88)
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))

;; Optional: show git changes in the gutter/fringe
;; Configure native-comp warnings before git-gutter
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors 'silent) ; Silence all native-comp warnings
  ;; Optional: if you want to only silence specific warnings
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; This setting is safe regardless of native-comp support
(setq native-comp-async-query-on-exit nil)

;; Git Gutter configuration
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Git Time Machine - Step through historic versions of git controlled files
(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine)
  :custom
  ;; Show commit info when switching revisions
  (git-timemachine-show-minibuffer-details t))

;; Ensure tree-sitter grammars are installed
(use-package treesit
  :straight (:type built-in)
  :config
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (sql "https://github.com/m-novikov/tree-sitter-sql")))

  ;; Auto-install grammars if they're missing
  (dolist (grammar treesit-language-source-alist)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar))))

  ;; Use tree-sitter modes when available
  ;; Set up remappings for modes that are commonly available
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (js-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (go-mode . go-ts-mode)))

  ;; For SQL mode, we need to ensure sql-mode is loaded before remapping
  ;; Otherwise we get "Ignoring unknown mode 'sql-mode'" errors
  (with-eval-after-load 'sql
    (when (and (treesit-language-available-p 'sql)
               (fboundp 'sql-ts-mode))
      (add-to-list 'major-mode-remap-alist '(sql-mode . sql-ts-mode))))

  ;; Ensure font-lock works well
  (setq treesit-font-lock-level 4))

;; Ensure we use built-in project package
(use-package project
  :straight (:type built-in)  ;; Tell straight.el this is a built-in package
  :demand t  ;; Load immediately to avoid conflicts
  :bind-keymap
  ("C-c p" . project-prefix-map)  ;; Similar to projectile's prefix
  :custom
  (project-list-file (locate-user-emacs-file "projects"))
  (project-vc-extra-root-markers '(".git" "pyproject.toml" "package.json"))
  (project-switch-commands 'project-find-file)
  (project-ignored-directories '(".venv" "node_modules" ".git"))
  (project-ignored-globs '("*.pyc" "*.o" "*.elc"))
  :config
  ;; Use ripgrep for project searches when available
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Set project search paths
  (when (file-directory-p "~/Projects")
    (setq project-switch-commands 'project-dired))

  ;; Custom function to use counsel-rg for project search
  (defun efs/project-search-with-counsel-rg ()
    "Search project using counsel-rg with live results (includes hidden files)."
    (interactive)
    (if-let* ((project (project-current))
              (root (project-root project)))
        (let ((default-directory root))
          (counsel-rg nil nil "--hidden"))
      (user-error "Not in a project")))

  ;; Search with --hidden --no-ignore (search everything including gitignored files)
  (defun efs/project-search-with-counsel-rg-all ()
    "Search project using counsel-rg with --hidden --no-ignore (searches all files)."
    (interactive)
    (if-let* ((project (project-current))
              (root (project-root project)))
        (let ((default-directory root))
          (counsel-rg nil nil "--hidden --no-ignore"))
      (user-error "Not in a project")))

  ;; Bind search to 's' in project keymap to use counsel-rg
  (define-key project-prefix-map "s" #'efs/project-search-with-counsel-rg)
  ;; Bind 'S' for search all (including gitignored files)
  (define-key project-prefix-map "S" #'efs/project-search-with-counsel-rg-all)

  ;; Add eat terminal in project root
  (defun efs/project-eat ()
    "Start or switch to an eat terminal in the project root directory."
    (interactive)
    (if-let* ((project (project-current))
              (root (project-root project))
              (project-name (project-name project))
              (buffer-name (format "*%s-eat*" project-name)))
        (if (get-buffer buffer-name)
            (pop-to-buffer buffer-name)
          (let ((default-directory root)
                (created-buffer))
            (eat)
            ;; Find the newly created eat buffer
            (setq created-buffer
                  (car (cl-remove-if-not
                        (lambda (buf)
                          (string-match-p "\\*eat\\*" (buffer-name buf)))
                        (buffer-list))))
            (when created-buffer
              (with-current-buffer created-buffer
                (rename-buffer buffer-name t)))))
      (user-error "Not in a project")))

  ;; Bind eat to 't' in project keymap
  (define-key project-prefix-map "t" #'efs/project-eat)

  ;; Add project management commands
  (define-key project-prefix-map "r" #'project-forget-project)
  (define-key project-prefix-map "z" #'project-forget-zombie-projects))

(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c C" . claude-code-command-map)
  :hook ((claude-code--start . sm-setup-claude-faces))
  :custom
  (claude-code-program "/Users/rod/.claude/local/claude")
  (claude-code-startup-delay 0.2)
  ;; (claude-code-newline-keybinding-style 'shift-return-to-send)
  :custom-face
  (claude-code-repl-face ((t (:family "JuliaMono"))))
  :config
  ;; (setq claude-code-terminal-backend 'vterm)
  
  ;; Custom function to create Claude buffer and switch to it
  (defun my/claude-code-and-switch ()
    "Start Claude Code in project root and switch to the buffer.
  If a Claude instance already exists for this directory, prompt for a new instance name."
    (interactive)
    (let* ((dir (claude-code--directory))
           (existing-buffers (claude-code--find-claude-buffers-for-directory dir)))
      (if existing-buffers
          ;; If buffers exist, prompt for instance name and create new instance
          (let ((instance-name (read-string "Instance name (leave empty for default): ")))
            (if (string-empty-p instance-name)
                ;; Use claude-code-new-instance without instance name
                (claude-code-new-instance '(4))
              ;; Use claude-code-new-instance with the provided instance name
              (let ((current-prefix-arg '(4)))
                (claude-code-new-instance instance-name))))
        ;; Otherwise use regular claude-code with switch
        (claude-code '(4)))))  ; Pass the correct prefix arg format to switch to buffer
  
  ;; Override the default C-c C c binding to use our custom function
  (define-key claude-code-command-map "c" #'my/claude-code-and-switch)
  
  ;; Override buffer naming to show only directory name, not full path
  (defun claude-code--buffer-name (&optional instance-name)
    "Generate the Claude buffer name showing only the directory name.
  
  If INSTANCE-NAME is provided, include it in the buffer name.
  If not in a project and no buffer file, raise an error."
    (let ((dir (claude-code--directory)))
      (if dir
          (let ((dir-name (file-name-nondirectory (directory-file-name dir))))
            (if instance-name
                (format "*claude:%s:%s*" dir-name instance-name)
              (format "*claude:%s*" dir-name)))
        (error "Cannot determine Claude directory - no `default-directory'!"))))
  
  ;; Fix auto-scrolling in eat terminal buffers used by claude-code
  (defun my/claude-code-eat-setup ()
    "Configure eat terminal for proper auto-scrolling in claude-code buffers."
    (when (and (derived-mode-p 'eat-mode)
               (string-match-p "\\*claude:" (buffer-name)))
      ;; Ensure point follows output
      (setq-local scroll-conservatively 101)
      (setq-local scroll-margin 0)
      ;; Move to end of buffer on new output
      (goto-char (point-max))))
  
  ;; Hook into eat-mode for claude buffers
  (add-hook 'eat-mode-hook #'my/claude-code-eat-setup)
  
  ;; Additional hook to ensure scrolling on process output
  (defun my/claude-code-scroll-to-bottom (&rest _)
    "Scroll claude-code eat buffers to bottom on new output."
    (when (and (derived-mode-p 'eat-mode)
               (string-match-p "\\*claude:" (buffer-name)))
      (let ((windows (get-buffer-window-list (current-buffer) nil t)))
        (dolist (window windows)
          (with-selected-window window
            (goto-char (point-max))
            (recenter -1))))))
  
  ;; Advise eat's process filter to ensure scrolling
  (with-eval-after-load 'eat
    (advice-add 'eat-self-input :after #'my/claude-code-scroll-to-bottom)
    (advice-add 'eat-term-redisplay :after #'my/claude-code-scroll-to-bottom))
  
  (claude-code-mode))

;; Claude Code IDE - Enhanced IDE features for Claude Code
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :after claude-code
  :config
  (claude-code-ide-mode 1)
  :bind
  ;; Use C-c B prefix for easy access
  (("C-c B m" . claude-code-ide-menu)  ; Main menu entry point
   ("C-c B e" . claude-code-ide-explain-code)
   ("C-c B i" . claude-code-ide-improve-code)
   ("C-c B d" . claude-code-ide-generate-docs)
   ("C-c B t" . claude-code-ide-generate-tests)
   ("C-c B r" . claude-code-ide-refactor)
   ("C-c B f" . claude-code-ide-fix-error)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key
        (auth-source-pick-first-password :host "api.openai.com"))
  (setq chatgpt-shell-anthropic-key
        (auth-source-pick-first-password :host "api.anthropic.com"))
  (setq chatgpt-shell-model-version "claude-3-7-sonnet-latest"))

;; Key bindings for ChatGPT shell commands
(global-set-key (kbd "C-c s") 'chatgpt-shell)
(global-set-key (kbd "C-c e") 'chatgpt-shell-prompt-compose)
(global-set-key (kbd "C-c m") 'chatgpt-shell-swap-model)

;; Dockerfile mode for editing Dockerfiles
(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; Docker management from Emacs
(use-package docker
  :ensure t
  :bind ("C-c d" . docker)
  :config
  (setq docker-command "docker"))

;; docker compose mode
(use-package docker-compose-mode
  :ensure t)

;; Python Development Configuration

;; Find programs in virtual env bin dir or relay on PATH
(defun efs/find-venv-directory (&optional start-dir)
  "Find .venv directory by traversing up from START-DIR.
If START-DIR is not provided, uses the current project root or default-directory."
  (let ((dir (or start-dir
                 (when (project-current)
                   (project-root (project-current)))
                 default-directory)))
    (while (and dir
                (not (equal dir "/"))
                (not (file-exists-p (expand-file-name ".venv" dir))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (when (and dir (file-exists-p (expand-file-name ".venv" dir)))
      (expand-file-name ".venv" dir))))

(defun efs/get-venv-program (program-name)
  "Get command for PROGRAM-NAME using project's virtualenv.
Returns a list containing the full path if found in virtualenv,
otherwise returns a list with just the program name.
Traverses up the directory tree to find .venv if not in project root."
  (let* ((venv-dir (efs/find-venv-directory))
         (venv-program (when venv-dir
                         (expand-file-name (concat "bin/" program-name) venv-dir))))
    (if (and venv-program (file-executable-p venv-program))
        (list venv-program)
      (list program-name))))

;; Function to activate virtualenv for current project
(defun efs/activate-venv ()
  "Activate the project's virtual environment by adding it to exec-path and PATH.
Traverses up the directory tree to find .venv if not in project root."
  (interactive)
  (let* ((venv-dir (efs/find-venv-directory))
         (venv-bin (when venv-dir
                     (expand-file-name "bin" venv-dir))))
    (when (and venv-bin (file-directory-p venv-bin))
      ;; Add to exec-path for Emacs to find executables
      (setq-local exec-path (cons venv-bin exec-path))
      ;; Update PATH environment variable for subprocesses
      (setenv "PATH" (concat venv-bin path-separator (getenv "PATH")))
      ;; Set VIRTUAL_ENV for tools that check for it
      (setenv "VIRTUAL_ENV" venv-dir)
      (message "Activated virtualenv: %s" venv-dir))))

;; Use treesit-based python mode when available
(use-package python
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-ts-mode)
  :custom
  (python-indent-offset 4)
  ;; Look for .venv by traversing up the directory tree
  (python-shell-virtualenv-root #'efs/find-venv-directory)
  :config
  ;; Function to get the project's virtualenv python
  (defun efs/get-project-python ()
    "Get python executable from project's virtualenv."
    (car (efs/get-venv-program "python")))

  ;; Set python shell interpreter dynamically
  (setq python-shell-interpreter #'efs/get-project-python)
  
  ;; Auto-activate virtualenv when opening Python files
  :hook ((python-mode . efs/activate-venv)
         (python-ts-mode . efs/activate-venv)))

;; Configure eglot for Python
(use-package eglot
  :straight (:type built-in)
  :hook ((python-ts-mode . (lambda ()
                             (message "[DEBUG] python-ts-mode hook called in: %s" default-directory)
                             (efs/activate-venv)
                             (message "[DEBUG] About to call eglot-ensure")
                             (eglot-ensure)
                             (message "[DEBUG] eglot-ensure completed")))
         (python-mode . (lambda ()
                         (message "[DEBUG] python-mode hook called in: %s" default-directory)
                         (efs/activate-venv)
                         (message "[DEBUG] About to call eglot-ensure")
                         (eglot-ensure)
                         (message "[DEBUG] eglot-ensure completed"))))
  :init (setq eglot-stay-out-of '(flymake))
  :custom
  (eglot-autoshutdown t)  ; Shutdown language server when buffer is closed
  (eglot-send-changes-idle-time 0.1)  ; How quickly to send changes to server
  (eglot-auto-display-help-buffer nil)  ; Don't automatically show help
  (eglot-sync-connect 3)  ; Give server 3 seconds to start
  (eglot-connect-timeout 10)  ; Connection timeout in seconds
  :config
  ;; Function to get virtualenv-aware jedi command
  (defun efs/get-jedi-command (&rest args)
    "Get jedi-language-server command using project's virtualenv.
  Accepts any number of arguments for eglot compatibility."
    (message "[DEBUG] efs/get-jedi-command called with args: %s" args)
    (message "[DEBUG] Current directory: %s" default-directory)
    (message "[DEBUG] Project current: %s" (project-current))

    ;; Log venv detection
    (let ((venv-dir (efs/find-venv-directory)))
      (message "[DEBUG] Found venv directory: %s" venv-dir))

    ;; First try to find in virtualenv, then fall back to system
    (let* ((venv-jedi (car (efs/get-venv-program "jedi-language-server")))
           (result))
      (message "[DEBUG] efs/get-venv-program returned: %s" venv-jedi)
      (message "[DEBUG] File executable check: %s" (and venv-jedi (file-executable-p venv-jedi)))

      (setq result
            (if (and venv-jedi (file-executable-p venv-jedi))
                (progn
                  (message "[DEBUG] Using virtualenv jedi: %s" venv-jedi)
                  (list venv-jedi))
              ;; If not found in venv, try pyls or pylsp as fallback
              (let ((venv-pylsp (car (efs/get-venv-program "pylsp"))))
                (if (and venv-pylsp (file-executable-p venv-pylsp))
                    (progn
                      (message "[DEBUG] Using virtualenv pylsp: %s" venv-pylsp)
                      (list venv-pylsp))
                  (let ((venv-pyls (car (efs/get-venv-program "pyls"))))
                    (if (and venv-pyls (file-executable-p venv-pyls))
                        (progn
                          (message "[DEBUG] Using virtualenv pyls: %s" venv-pyls)
                          (list venv-pyls))
                      (progn
                        (message "[DEBUG] Falling back to system jedi-language-server")
                        (list "jedi-language-server"))))))))

      (message "[DEBUG] efs/get-jedi-command returning: %s" result)
      result))

  ;; Remove default Python server programs to avoid conflicts
  (message "[DEBUG] Before cleanup, eglot-server-programs has %d entries" (length eglot-server-programs))
  (let ((python-entries (cl-count-if (lambda (entry)
                                       (and (listp (car entry))
                                            (or (memq 'python-mode (car entry))
                                                (memq 'python-ts-mode (car entry)))))
                                     eglot-server-programs)))
    (message "[DEBUG] Found %d Python entries to remove" python-entries))
  
  (setq eglot-server-programs
        (cl-remove-if (lambda (entry)
                        (and (listp (car entry))
                             (or (memq 'python-mode (car entry))
                                 (memq 'python-ts-mode (car entry)))))
                      eglot-server-programs))
  
  (message "[DEBUG] After cleanup, eglot-server-programs has %d entries" (length eglot-server-programs))
  
  ;; Register our custom jedi command
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) . efs/get-jedi-command))
  
  (message "[DEBUG] Registered custom jedi command. Final count: %d entries" (length eglot-server-programs)))

;; Format Python code with ruff
(use-package reformatter
  :config
  ;; Function to get ruff formatter command
  (defun efs/get-ruff-command ()
    "Get ruff command from virtualenv or global install."
    (car (efs/get-venv-program "ruff")))

  ;; Define formatters for both format and isort
  (reformatter-define ruff-format
    :program (efs/get-ruff-command)
    :args '("format" "-"))

  (reformatter-define ruff-isort
    :program (efs/get-ruff-command)
    :args '("check" "--select" "I" "--fix" "-"))

  ;; Combined formatting function
  (defun ruff-format-and-sort ()
    "Run ruff format and import sorting on current buffer."
    (interactive)
    (ruff-isort-buffer)
    (ruff-format-buffer))

  ;; Hook to run both on save
  :hook ((python-ts-mode . (lambda ()
                             (add-hook 'before-save-hook #'ruff-format-and-sort nil t)))
         (python-mode . (lambda ()
                          (add-hook 'before-save-hook #'ruff-format-and-sort nil t)))))

;; Configure pytest integration
(use-package python-pytest
  :after python
  :custom
  (python-pytest-confirm t)
  :bind
  (:map python-ts-mode-map
        ("C-c C-x t" . python-pytest-dispatch))
  (:map python-mode-map
        ("C-c C-x t" . python-pytest-dispatch)))

;; Legacy support for poetry projects
(use-package poetry
  :after python
  :config
  (poetry-tracking-mode))

;; Improved Python docstring editing
(use-package python-docstring
  :hook ((python-ts-mode python-mode) . python-docstring-mode))

;; Advanced Python folding
(use-package origami
  :hook ((python-ts-mode python-mode) . origami-mode))

;; Python Linting Configuration
(use-package flymake
  :straight (:type built-in)
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-save-buffer t)
  (flymake-no-changes-timeout 0.3)
  :config
  ;; Show flymake diagnostics first in minibuffer
  (setq eldoc-documentation-functions
        (cons #'flymake-eldoc-function
              (remove #'flymake-eldoc-function eldoc-documentation-functions)))
  :hook ((python-ts-mode . flymake-mode)
         (python-mode . flymake-mode)))

(use-package flymake-ruff
  :straight (flymake-ruff :type git :host github :repo "erickgnavar/flymake-ruff")
  :config
  ;; Dynamically set ruff program based on current buffer's project
  (defun efs/setup-flymake-ruff ()
    "Setup flymake-ruff with virtualenv-aware ruff path."
    (setq-local flymake-ruff-program (car (efs/get-venv-program "ruff"))))
  :hook ((python-ts-mode . (lambda ()
                             (efs/setup-flymake-ruff)
                             (flymake-ruff-load)))
         (python-mode . (lambda ()
                          (efs/setup-flymake-ruff)
                          (flymake-ruff-load)))))

;; DAP Mode for debugging
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  ;; Use debugpy for Python debugging
  (setq dap-python-debugger 'debugpy)
  ;; Get debugpy executable from virtualenv
  (setq dap-python-executable (car (efs/get-venv-program "python")))
  ;; Default to a fixed port
  (setq dap-python-default-debug-port 5678))

;; SQL Mode Configuration
;; Note, you'll need
;; # For Ubuntu/Debian
;; sudo apt install pgformatter
;; # For MacOS
;; brew install pgformatter

;; Basic SQL Mode
(use-package sql
  :straight (:type built-in)
  :mode ("\\.sql\\'" . sql-mode)  ;; Regular mapping, tree-sitter handled by remap
  :init
  ;; Ensure sql-ts-mode is available before use
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'sql))
    (require 'sql-ts-mode nil t))
  :custom
  (sql-product 'postgres)  ; Default to PostgreSQL
  (sql-indent-offset 2)
  :config
  ;; Log SQL tree-sitter status
  (message "SQL tree-sitter status: language-available=%s, sql-ts-mode-defined=%s"
           (and (fboundp 'treesit-language-available-p)
                (treesit-language-available-p 'sql))
           (fboundp 'sql-ts-mode))
  ;; Load connection configuration if it exists
  (when (file-exists-p (expand-file-name "sql-connections.el" user-emacs-directory))
    (load (expand-file-name "sql-connections.el" user-emacs-directory)))

  ;; Helper function for SQL connections
  (defun efs/sql-connect-preset (name)
    "Connect to a predefined SQL connection by NAME."
    (interactive
     (list
      (completing-read "SQL connection: "
                       (mapcar #'car sql-connection-alist))))
    (let ((connection (assoc name sql-connection-alist)))
      (when connection
        (setq sql-connection-alist (cons connection (delete connection sql-connection-alist)))
        (let ((sql-product (cadr (assoc 'sql-product connection))))
          (sql-connect name)))))

  ;; Helper function to set dialect based on file extension or buffer name
  (defun efs/sql-set-dialect-from-file ()
    "Set SQL dialect based on file extension or buffer name."
    (let ((file-name (buffer-file-name))
          (buffer-name (buffer-name)))
      (cond
       ;; By file extension
       ((and file-name (string-match "\\.psql\\'" file-name)) (sql-set-product 'postgres))
       ((and file-name (string-match "\\.mysql\\'" file-name)) (sql-set-product 'mysql))
       ((and file-name (string-match "\\.sqlite\\'" file-name)) (sql-set-product 'sqlite))
       ;; By buffer naming conventions
       ((and buffer-name (string-match "postgres\\|pg_\\|pgsql" buffer-name)) (sql-set-product 'postgres))
       ((and buffer-name (string-match "mysql" buffer-name)) (sql-set-product 'mysql))
       ((and buffer-name (string-match "sqlite" buffer-name)) (sql-set-product 'sqlite))))))

;; SQLi history configuration
(use-package sql
  :straight (:type built-in)
  :custom
  (sql-input-ring-file-name (expand-file-name "sqli_history" no-littering-var-directory))
  (sql-input-ring-size 1000)
  :hook
  ;; Set dialect on file open
  (sql-mode . efs/sql-set-dialect-from-file)
  (sql-interactive-mode . (lambda ()
                            (toggle-truncate-lines t)
                            (sql-input-ring-load)
                            (add-hook 'kill-buffer-hook 'sql-input-ring-save nil t))))

;; SQL indentation
(use-package sql-indent
  :hook ((sql-mode sql-ts-mode) . sqlind-minor-mode)
  :custom
  (sqlind-basic-offset 2)
  (sqlind-indentation-offsets-alist
   '((select-clause 0)
     (insert-clause 0)
     (delete-clause 0)
     (update-clause 0)
     (select-column-continuation + sqlind-basic-offset)
     (select-join-condition + sqlind-basic-offset)
     (select-table (sqlind-lineup-joins-to-anchor sqlind-basic-offset 1))
     (in-select-clause sqlind-lineup-select-target)
     (in-select-join-condition sqlind-lineup-select-join)
     (in-select-column sqlind-lineup-list-item)
     (select-table-continuation + sqlind-basic-offset))))

;; Enable sqlup-mode for SQL keyword capitalization
(use-package sqlup-mode
  :hook ((sql-mode sql-interactive-mode sql-ts-mode) . sqlup-mode))

;; SQL Mode company integration
(with-eval-after-load 'company
  (add-hook 'sql-mode-hook
            (lambda ()
              (setq-local company-backends
                          (append '(company-keywords company-dabbrev-code)
                                  company-backends))))
  (add-hook 'sql-ts-mode-hook
            (lambda ()
              (setq-local company-backends
                          (append '(company-keywords company-dabbrev-code)
                                  company-backends)))))

;; Format SQL with sqlformat
(use-package sqlformat
  :custom
  (sqlformat-command 'pgformatter)    ; 'sqlformat, 'pgformatter, or 'sqlfluff
  (sqlformat-args '("-s2" "-g"))      ; Arguments for pgformatter
  :hook
  ((sql-mode sql-ts-mode) . (lambda ()
                            (add-hook 'before-save-hook 'sqlformat-buffer nil t))))

(use-package dotenv-mode
  :mode ("\\.env\\(\\..*\\)?\\'" . dotenv-mode))
