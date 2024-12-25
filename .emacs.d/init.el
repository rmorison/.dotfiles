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

;; line and column numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
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
  :init (load-theme 'doom-acario-dark t))

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
  :config
  (counsel-mode 1))

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
   ("^"   . dirvish-history-last)
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
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (js-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (go-mode . go-ts-mode)
          (sql-mode . sql-ts-mode)))

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
  (project-vc-extra-root-markers '("pyproject.toml" "package.json"))
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

  ;; Bind search to 's' in project keymap
  (define-key project-prefix-map "s" #'project-find-regexp)

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
  (define-key project-prefix-map "t" #'efs/project-eat))

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

;; Python Development Configuration
(defun efs/python-mode-setup ()
  "Setup for Python development environment."
  (setq-local indent-tabs-mode nil)
  (when (fboundp 'python-ts-mode)
    (python-ts-mode))
  (setq eldoc-mode t))

;; Virtual environment management
(use-package pyvenv
  :after python
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/.pyenv/versions"))
  :config
  ;; Display virtual env in mode line
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:%s]" pyvenv-virtual-env-name)))

  ;; Helper function to find and activate virtualenv
  (defun efs/auto-activate-virtualenv ()
    "Automatically activate virtualenv in the current project."
    (interactive)
    (let* ((project-dir (locate-dominating-file default-directory ".venv"))
           (venv-path (when project-dir (expand-file-name ".venv" project-dir))))
      (when (and venv-path (file-directory-p venv-path))
        (pyvenv-activate venv-path)
        ;; Restart Eglot if it's running to pick up the new virtualenv
        (when (and (bound-and-true-p eglot--managed-mode)
                   (eglot-current-server))
          (eglot-shutdown)
          (eglot)))))

  ;; Auto-activate for Python modes
  (add-hook 'python-mode-hook #'efs/auto-activate-virtualenv)
  (add-hook 'python-ts-mode-hook #'efs/auto-activate-virtualenv))

;; Configure Eglot with Ruff
(use-package eglot
  :straight (:type built-in)  ; Eglot is built into Emacs 29+
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config
  ;; Function to get server path
  (defun efs/get-virtualenv-ruff (server-name)
    "Get path to ruff executable in current virtualenv or fall back to global.
     Argument SERVER-NAME is required by Eglot but unused."
    (let ((ruff-path (if pyvenv-virtual-env
                         (expand-file-name "bin/ruff" pyvenv-virtual-env)
                       (executable-find "ruff"))))
      (when ruff-path
        (list ruff-path "server"))))

  ;; Interactive format functions
  (defun efs/format-python-buffer ()
    "Format the current Python buffer using Eglot."
    (interactive)
    (if (and (bound-and-true-p eglot--managed-mode)
             (eglot-current-server))
        (eglot-format-buffer)
      (message "Eglot is not running. Start it with M-x eglot")))

  (defun efs/python-sort-imports ()
    "Sort Python imports using Ruff."
    (interactive)
    (if (and (bound-and-true-p eglot--managed-mode)
             (eglot-current-server))
        (let* ((current-file (buffer-file-name))
               (ruff-path (if pyvenv-virtual-env
                             (expand-file-name "bin/ruff" pyvenv-virtual-env)
                           (executable-find "ruff")))
               (output-buffer (generate-new-buffer "*ruff-output*")))
          (if (and current-file ruff-path)
              (progn
                ;; Save buffer first if modified
                (when (buffer-modified-p)
                  (save-buffer))
                ;; Run ruff with correct command syntax
                (if (zerop (call-process ruff-path nil output-buffer t 
                                       "check"
                                       "--select" "I001" 
                                       "--fix"
                                       current-file))
                    (progn
                      ;; Force a reread from disk
                      (revert-buffer t t t)
                      (message "Imports sorted successfully"))
                  (with-current-buffer output-buffer
                    (message "Ruff error: %s" (buffer-string))))
                (kill-buffer output-buffer))
            (message "Unable to find ruff or current buffer is not visiting a file")))
      (message "Eglot is not running. Start it with M-x eglot")))

  ;; Configure Eglot to use Ruff
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ,#'efs/get-virtualenv-ruff))

  ;; Key bindings for Python modes
  (define-key python-mode-map (kbd "C-c f b") #'efs/format-python-buffer)
  (define-key python-mode-map (kbd "C-c f i") #'efs/python-sort-imports)

  ;; Also add bindings for python-ts-mode
  (define-key python-ts-mode-map (kbd "C-c f b") #'efs/format-python-buffer)
  (define-key python-ts-mode-map (kbd "C-c f i") #'efs/python-sort-imports)

;; Format and sort imports on save
(add-hook 'before-save-hook
          (lambda ()
            (when (and (bound-and-true-p eglot--managed-mode)
                       (or (derived-mode-p 'python-mode)
                           (derived-mode-p 'python-ts-mode)))
              ;; First format with eglot
              (eglot-format-buffer)
              ;; Then run ruff sort imports directly without saving
              (let* ((current-file (buffer-file-name))
                     (ruff-path (if pyvenv-virtual-env
                                   (expand-file-name "bin/ruff" pyvenv-virtual-env)
                                 (executable-find "ruff"))))
                (when (and current-file ruff-path)
                  (call-process ruff-path nil nil nil 
                              "check"
                              "--select" "I001" 
                              "--fix"
                              current-file)
                  ;; Revert the buffer to pick up ruff's changes
                  ;; but don't trigger another save
                  (revert-buffer t t t))))))

  ;; Configure Eglot to show diagnostics immediately
  (setq eglot-send-changes-idle-time 0)
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect nil)
  (setq eglot-connect-timeout 10)
  (setq eglot-autoshutdown t))

;; Debug function for Python development setup
(defun efs/debug-python-dev-config ()
  "Debug Python development configuration."
  (interactive)
  (let* ((project-root (and (project-current) (project-root (project-current))))
         (pyproject-path (when project-root (expand-file-name "pyproject.toml" project-root)))
         (venv-path pyvenv-virtual-env)
         (venv-ruff (when venv-path 
                     (expand-file-name "bin/ruff" venv-path)))
         (global-ruff (executable-find "ruff"))
         (active-ruff (if (and venv-path (file-exists-p venv-ruff))
                         venv-ruff
                       global-ruff))
         (eglot-server (when (bound-and-true-p eglot--managed-mode)
                        (eglot--server-name (eglot-current-server)))))

    ;; Print debug information
    (with-current-buffer (get-buffer-create "*python-dev-debug*")
      (erase-buffer)
      (insert "Python Development Configuration Debug Info:\n\n")
      (insert (format "Project Root: %s\n" project-root))
      (insert (format "pyproject.toml exists: %s\n" (and pyproject-path (file-exists-p pyproject-path))))
      (insert (format "pyproject.toml path: %s\n" pyproject-path))
      (insert (format "Virtual Env: %s\n" venv-path))
      (insert (format "Virtualenv Ruff: %s\n" venv-ruff))
      (insert (format "Global Ruff: %s\n" global-ruff))
      (insert (format "Active Ruff: %s\n" active-ruff))
      (insert (format "Eglot running: %s\n" (bound-and-true-p eglot--managed-mode)))
      (insert (format "Eglot server: %s\n" eglot-server))
      (insert (format "Current server: %s\n" (and (bound-and-true-p eglot--managed-mode)
                                                 (eglot-current-server))))

      ;; Try to read pyproject.toml content if it exists
      (when (and pyproject-path (file-exists-p pyproject-path))
        (insert "\npyproject.toml content:\n")
        (insert-file-contents pyproject-path)
        (goto-char (point-max))))

    (display-buffer "*python-dev-debug*")))

;; Add debug hook for Python modes
(defun efs/python-mode-debug-hook ()
  "Hook to run python debug info when mode starts."
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (efs/debug-python-dev-config)))

(add-hook 'python-mode-hook #'efs/python-mode-debug-hook)
(add-hook 'python-ts-mode-hook #'efs/python-mode-debug-hook)
