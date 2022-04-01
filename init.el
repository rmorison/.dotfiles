
;; MacOS key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  ;; brew install svn
  ;; brew tap homebrew/cask-fonts
  ;; brew install --cask font-fira-code font-fira-mono
  ;; brew install --cask font-cantarell
  ;; brew install coreutils
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  )
 
;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)
(defvar efs/default-font-family "Fira Code")
(defvar efs/fixed-font-family "Fira Code")
(defvar efs/variable-font-family "Cantarell")

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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons-dired dired-single eshell-git-prompt visual-fill-column org-bullets magit counsel-projectile projectile helpful rainbow-delimiters doom-modeline all-the-icons doom-themes command-log-mode ivy-rich counsel ivy which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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

  (setq org-directory "~/Projects/org")
  (setq org-agenda-files '(org-directory))

  (setq org-agenda-compact-blocks t)

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("reference.org" "people.org" "projects.org" :maxlevel . 9)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  
  (setq org-todo-keywords
	'((sequence "TODO(t)" "ONDECK(o)" "ATBAT(@)" "BLOCKED(b)" "|" "DONE(d)" "ICEBOX(i)")
	  (sequence "PLAN(p)" "READY(r)" "ACTIVE(a)" "HELD(h)" "|" "COMPLETE(c)" "MOTHBALLED(m)")))
  
  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
	  (:endgroup)
	  ("project" . ?p)
	  ("agenda" . ?a)
	  ("meeting" . ?m)
	  ("key-concept" . ?k)
	  ("note" . ?n)
	  ("idea" . ?i)))
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
	    (todo "ACTIVE" ((org-agenda-overriding-header "Active projects")))
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
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
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
	  
	  ("j" "Journal" entry (file+olp+datetree "journal.org")
	   (file "templates/journal.org")
	   :tree-type week)
	   
	  ("m" "Meeting" entry (file+olp+datetree "meetings.org")
	   (file "templates/meeting.org")
	   :tree-type week)))

  (efs/org-font-setup))

;; org mode key bindings
(define-key global-map (kbd "C-c c")
  (lambda () (interactive) (org-capture nil)))
;;(global-set-key (kbd "\C-cc") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

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
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

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

(when (eq system-type 'darwin) ;; mac specific settings
  (message "adding %s inits" (system-name))

  ;; mysql v5.7
  (setenv "PATH" (concat "/usr/local/opt/mysql-client@5.7/bin:/usr/local/MacGPG2/bin:/usr/local/bin:" (getenv "PATH")))
  (setq exec-path (append '("/usr/local/opt/mysql-client@5.7/bin") '("/usr/local/MacGPG2/bin") '("/usr/local/bin") exec-path))

  ;; Java
  (setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home")

  ;; Docket
  ;; (setenv "DOCKER_TLS_VERIFY" "1")
  ;; (setenv "DOCKER_HOST" "tcp://192.168.99.100:2376")
  ;; (setenv "DOCKER_CERT_PATH" "/Users/rmorison/.docker/machine/machines/default")
  ;; (setenv "DOCKER_MACHINE_NAME" "default")
  ;; (setenv "DOCKER_HOST_IP" (url-host (url-generic-parse-url (getenv "DOCKER_HOST"))))

  ;; pyenv
  (setenv "PATH" (concat "/Users/rmorison/.local/bin:/Users/rmorison/.pyenv/shims:/Users/rmorison/.pyenv/bin:" (getenv "PATH")))
  (setq exec-path (append '("/Users/rmorison/.local/bin") '("/Users/rmorison/.pyenv/shims:/Users/rmorison/.pyenv/bin") exec-path))

  ;; nvm
  (setenv "NVM_DIR" "/Users/rmorison/.nvm")
  (setenv "NVM_CD_FLAGS" "-q")
  (setenv "NVM_RC_VERSION" "")
  (setenv "NVM_BIN" "/Users/rmorison/.nvm/versions/node/v16.14.0/bin")
  (setenv "NVM_INC" "/Users/rmorison/.nvm/versions/node/v16.14.0/include/node")
  (setenv "PATH" (concat "/Users/rmorison/.nvm/versions/node/v16.14.0/bin:" (getenv "PATH")))
  (setq exec-path (append '("/Users/rmorison/.nvm/versions/node/v16.14.0/bin") exec-path)))
