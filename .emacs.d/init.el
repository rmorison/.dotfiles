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

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)
(defvar efs/default-font-family "Fira Code")
(defvar efs/fixed-font-family "Fira Code")
(defvar efs/variable-font-family "Cantarell")

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
(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024))

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

(setq global-auto-revert-mode 't)

;;(load-theme 'tango-dark)
;; have tried: doom-palenight doom-material-dark doom-solarized-light doom-solarized-light doom-zenburn doom-monokai-machine doom-oceanic-next
(use-package doom-themes
  :init (load-theme 'doom-one t))

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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; from https://www.fettesps.com/emacs-disable-suspend-button/
;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Windows Style Undo
(global-set-key [(control z)] 'undo)

;; Comment toggle
(global-set-key (kbd "C-c C-.") 'comment-or-uncomment-region)

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
                                   "hugo")))

  (eshell-git-prompt-use-theme 'powerline))

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
  (setq org-agenda-files '("~/org" "~/Blogs/rmorison.github.io/org"))

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
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!)" "DELEGATED(D@)" "HELD-BLOCKED(h@/!)" "|" "DONE(d!)" "WONT-DO(w@)")
          (sequence "BREAKDOWN(b)" "READY(r)" "ACTIVE(a!)" "WATCHING(W@)" "HELD-BLOCKED(h@/!)" "|" "DONE(d!)" "WONT-DO(w@)")))

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

  ;; don't ask on eval block C-c C-c
  (setq org-confirm-babel-evaluate nil))

;; org mode key bindings
(define-key global-map (kbd "C-c c")
  (lambda () (interactive) (org-capture nil)))
;;(global-set-key (kbd "\C-cc") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "↪" "→" "○" "●" "✸" "✿" "•" "★" "•" "★" "•" "★")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package ob-go)

(use-package ob-mermaid
  :config
  (setq ob-mermaid-cli-path "/home/rod/.npm/_npx/668c188756b835f3/node_modules/.bin/mmdc"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (mermaid . t)
   (shell . t)
   (python . t)
   (go . t)))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/Projects/github.com/rmorison/dotfiles/.emacs.d/Emacs.org"))
    ;; Dyname scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)

(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

(put 'upcase-region 'disabled nil)

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
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  (magit-branch-read-upstream-first 'fallback))

;; flycheck syntax checker
(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil))

;; language servers, language setups
;; see https://emacs-lsp.github.io/lsp-mode/page/languages/ for lsp support
(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (setq lsp-log-io t)
  (electric-pair-mode 1))

;; lsp, dap mode tips
;; typescript: npm install -g typescript-language-server; npm install -g typescript
;; python: pipenv install --dev black mypy debugpy pylint python-lsp-server \
;;           python-lsp-black pyls-isort isort pylsp-mypy flake8
;; notes:
;; - pyls-flake8 breaks pylsp flake8 handling
;; - helpful lsp debug notes at https://www.mattduck.com/lsp-python-getting-started.html

;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-mode-setup)
         ;;(typescript-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (go-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-snippet nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-doc-enable t
              lsp-ui-peek-enable t
              lsp-ui-sideline-enable t
              lsp-ui-imenu-enable t
              lsp-ui-flycheck-enable t)
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

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

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :commands yas-minor-mode
  :hook ((go-mode . yas-minor-mode)
         (python-mode . yas-minor-mode)))

;; nvm needs special help with PATH
(setq nvm/dir (concat (getenv "HOME") "/.nvm/versions/node/v16.14.0"))
(setenv "NVM_DIR" nvm/dir)
(setenv "NVM_CD_FLAGS" "-q")
(setenv "NVM_RC_VERSION" "")
(setenv "NVM_BIN" (concat nvm/dir "/bin"))
(setenv "NVM_INC" (concat nvm/dir "/include/node"))
(setenv "PATH" (concat (getenv "NVM_BIN") ":" (getenv "PATH")))

;; DAP
(use-package dap-mode
  :commands dap-debug
  :init (setq dap-print-io t)
  :config
  (dap-ui-mode 1)
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  ;; Bind `C-c l d` to `dap-hydra` for easy access

  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")))

;; Golang

;; env & path for https://github.com/stefanmaric/g
(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "GOROOT" (concat (getenv "HOME") "/.go"))

(use-package go-rename)
(use-package golint)

(defun go-mode-setup ()
  ;; (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  ;; (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  ;; (setq compilation-read-command t)
  (define-key (current-local-map) (kbd "C-c C-c") 'compile)
  (setq tab-width 4)
  (dap-register-debug-template
   "Launch Go Test File"
   (list :type "go"
         :request "launch"
         :name "Launch Go Test File"
         :mode "test"
         :program nil
         :buildFlags nil
         :args nil
         :env nil)))

(use-package go-mode
  :hook (go-mode . go-mode-setup)
  :config
  (require 'dap-hydra)
  (require 'dap-dlv-go))

;; pyenv, pipenv, teach dap where to find virtualenv python
(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package with-venv)

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
  :custom
  (python-black-command (with-venv (executable-find "black")))
  (python-black-on-save-mode 't))

(defun dap-python-setup()
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python"))))

(add-hook 'dap-mode-hook 'dap-python-setup)

(defun python-mode-setup()
  (flycheck-mode)
  (add-hook 'before-save-hook 'lsp-format-buffer)
  (setq lsp-pylsp-plugins-flake8-enabled 't)
  (dap-register-debug-template
   "python :: workspace"
   (list :name "python :: workspace"
         :type "python"
         :args ""
         :cwd "${workspaceFolder}"
         :env '(("PYTHONPATH" . "${workspaceFolder}"))
         :request "launch"
         :jinja "true"))
  (dap-register-debug-template
   "pytest :: workspace"
   (list :name "pytest :: workspace"
         :type "python"
         :args ""
         :cwd "${workspaceFolder}"
         :env '(("PYTHONPATH" . "${workspaceFolder}"))
         :program (with-venv (executable-find "pytest"))
         :request "launch"
         :jinja "true")))

(use-package python-mode
  :ensure t
  :hook (python-mode . python-mode-setup)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  (lsp-pylsp-server-command (with-venv (executable-find "pylsp"))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(add-hook 'html-mode-hook 'lsp-deferred)
(add-hook 'js-mode-hook 'lsp-deferred)
(setq js-indent-level 2)

(use-package protobuf-mode)
