;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)
(defvar efs/default-font-family "Fira Code")
(defvar efs/fixed-font-family "Fira Code")
(defvar efs/variable-font-family "Cantarell")

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
  (setq efs/default-variable-font-size 160)

  ;; mysql v5.7
  (setenv "PATH" (concat "/usr/local/opt/mysql-client@5.7/bin:/usr/local/MacGPG2/bin:/usr/local/bin:/usr/local/Cellar/libpq/14.2/bin:" (getenv "PATH")))
  (setq exec-path (append '("/usr/local/opt/mysql-client@5.7/bin") '("/usr/local/MacGPG2/bin") '("/usr/local/bin") '("/usr/local/Cellar/libpq/14.2/bin") exec-path))

  ;; Java
  (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home"))
 
;; Make frame transparency overridable
(defvar efs/frame-transparency '(90 . 90))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; top level look/feel
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

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

(set-face-attribute 'default nil :font efs/default-font-family :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font efs/fixed-font-family :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font efs/variable-font-family :height efs/default-variable-font-size :weight 'regular)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dir-treeview dockerfile-mode yaml-mode ox-gfm python-mode with-venv pipenv pyvenv typescript-mode company-box flycheck company dap-mode lsp-ivy lsp-treemacs lsp-ui lsp-mode exec-path-from-shell all-the-icons-dired dired-single eterm-256color eshell-git-prompt visual-fill-column org-bullets magit counsel-projectile projectile helpful rainbow-delimiters doom-modeline all-the-icons doom-themes command-log-mode ivy-rich counsel ivy which-key general no-littering use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-partial-or-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
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

(use-package command-log-mode
  :commands command-log-mode)

;;(load-theme 'tango-dark)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

;; NOTE: If icons are missing run following command:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-branch-read-upstream-first 'fallback))

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
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-image-actual-width (list 640))

  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org"))

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
	'((sequence "TODO(t)" "ONDECK(o)" "ATBAT(a)" "BLOCKED(b)" "|" "DONE(d)" "ICEBOX(i)")
	  (sequence "PLAN(p)" "READY(r)" "LIVE(l)" "HELD(h)" "|" "COMPLETE(c)" "MOTHBALLED(m)")))

  (setq org-todo-keyword-faces
      (quote (("TODO" :foreground "orange" :weight bold)
              ("ONDECK" :foreground "light blue" :weight bold)
              ("ATBAT" :foreground "forest green" :weight bold)
              ("BLOCKED" :foreground "red" :weight bold)
              ("DONE" :foreground "white" :weight bold)
              ("ICEBOX" :foreground "grey" :weight bold)
              ("PLAN" :foreground "orange" :weight bold)
              ("READY" :foreground "light blue" :weight bold)
              ("LIVE" :foreground "forest green" :weight bold)
              ("HELD" :foreground "red" :weight bold)
              ("COMPLETE" :foreground "white" :weight bold)
              ("MOTHBALLED" :foreground "grey" :weight bold))))
  
  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("project" . ?p)
	  ("agenda" . ?a)
	  ("meeting" . ?m)
	  ("key-concept" . ?k)
	  ("note" . ?n)
	  ("idea" . ?i)
	  ("goal" . ?g)))
  (setq org-fast-tag-selection-single-key t)
  
  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "BLOCKED" ((org-agenda-overriding-header "Stuck tasks")))
	    (todo "ONDECK" ((org-agenda-overriding-header "Tasks on deck")))
	    (todo "ATBAT" ((org-agenda-overriding-header "Tasks in play")))
	    (todo "TODO" ((org-agenda-overriding-header "Task backlog")))
	    (todo "HELD" ((org-agenda-overriding-header "Projects on hold")))
	    (todo "READY" ((org-agenda-overriding-header "Ready to start projects")))
	    (todo "LIVE" ((org-agenda-overriding-header "Live projects")))
	    (todo "PLAN" ((org-agenda-overriding-header "Projects that need planning")))))

	  ("b" "Backlog triage"
	   ((todo "TODO"
		  ((org-agenda-overriding-header "Tasks to put on deck")))))

	  ("@" "On deck and at bat"
	   ((todo "ONDECK"
		  ((org-agenda-overriding-header "Now hitting or on deck")))))

	  ("c" "Completed tasks and projects"
	   ((todo "DONE"
		  ((org-agenda-overriding-header "Tasks done"))))
	   ((todo "COMPLETE"
		  ((org-agenda-overriding-header "Projects complete")))))

	  ("i" "Iced tasks and projects"
	   ((todo "ICEBOX"
		  ((org-agenda-overriding-header "Tasks on ice"))))
	   ((todo "MOTHBALLED"
		  ((org-agenda-overriding-header "Mothballed projects")))))

	  ("p" "Project Status"
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "LIVE"
		  ((org-agenda-overriding-header "Live Projects")
		   (org-agenda-files org-agenda-files)))
	   ((todo "HOLD"
		  ((org-agenda-overriding-header "Projects on hold")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "KILLED"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  ;; Define capture templates
  (setq org-capture-templates
	`(("t" "Task" entry (file+headline "inbox.org" "Tasks")
           (file "templates/task.org"))

	  ("p" "Project" entry (file+headline "inbox.org" "Projects")
           (file "templates/project.org"))
	  
	  ("n" "Note" entry (file+headline "inbox.org" "Notes")
           (file "templates/note.org"))
	  
	  ("N" "Private note" entry (file "private.org")
           (file "templates/note.org"))
	  
	  ("j" "Journal" entry (file+olp+datetree "journal.org")
	   (file "templates/journal.org")
	   :tree-type week)
	   
	  ("m" "Meeting" entry (file+olp+datetree "meetings.org")
	   (file "templates/meeting.org")
	   :tree-type week)

	  ("1" "1-1 Meeting" entry (file+headline "inbox.org" "1-1 Meetings")
	   (file "templates/meeting.org"))))

  (efs/org-font-setup))

;; org mode code blocks
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; org mode key bindings
(define-key global-map (kbd "C-c c")
  (lambda () (interactive) (org-capture nil)))
;;(global-set-key (kbd "\C-cc") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "↪" "→" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package ox-gfm)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package dockerfile-mode)

(use-package dir-treeview
  :config
  (load-theme 'dir-treeview-pleasant t))
(global-set-key (kbd "<f9>") 'dir-treeview)

;; eshell
(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
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
    (setq eshell-visual-commands '("htop" "zsh" "vim" "ntl" "netlify" "ipython" "psql" "ssh" "mysql" "poetry" "docker")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; make sure ncurses package is installed
;; test: echo "Hello world" | cowsay | lolcat -p 0.7
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; dired
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single)
  ;; :commands (dired dired-jump)
  ;; :custom
  ;; (dired-single-use-magic-buffer t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; nvm needs special help with PATH
(setq nvm/dir (concat (getenv "HOME") "/.nvm/versions/node/v16.14.0"))
(setenv "NVM_DIR" nvm/dir)
(setenv "NVM_CD_FLAGS" "-q")
(setenv "NVM_RC_VERSION" "")
(setenv "NVM_BIN" (concat nvm/dir "/bin"))
(setenv "NVM_INC" (concat nvm/dir "/include/node"))
(setenv "PATH" (concat (getenv "NVM_BIN") ":" (getenv "PATH")))
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; language servers, language setups
;; see https://emacs-lsp.github.io/lsp-mode/page/languages/ for lsp support
(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (setq lsp-log-io t))

;; lsp, dap mode tips
;; typescript: npm install -g typescript-language-server; npm install -g typescript
;; python: pipenv install --dev black mypy debugpy pylint python-lsp-server \
;;           python-lsp-black pyls-isort isort pylsp-mypy flake8
;; notes:
;; - pyls-flake8 breaks pylsp flake8 handling
;; - helpful lsp debug notes at https://www.mattduck.com/lsp-python-getting-started.html

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (setq lsp-enable-snippet nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; npm install -g eslint
(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil)
  )

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(add-hook 'html-mode-hook 'lsp-deferred)
(add-hook 'js-mode-hook 'lsp-deferred)
(setq js-indent-level 2)

;; pyenv, pipenv, teach dap where to find virtualenv python
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package with-venv)

(defun python-mode-setup()
  (lsp-deferred)
  (flycheck-mode)
  (add-hook 'before-save-hook 'lsp-format-buffer)
  ;; lsp-pylsp init
  (setq lsp-pylsp-plugins-flake8-enabled 't)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv
      (executable-find command))))

(use-package python-mode
  :ensure t
  :hook (python-mode . python-mode-setup)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  (lsp-pylsp-server-command (with-venv (executable-find "pylsp")))
  :config
  (require 'dap-python))
