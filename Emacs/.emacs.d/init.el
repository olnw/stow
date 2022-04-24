;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)))

(set-default-coding-systems 'utf-8)

(setq user-emacs-directory "~/stow/Emacs/.emacs.d/")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package and use it by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package bind-key
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

(use-package no-littering)

;; Store backup and auto-save files in the var/ directory
(setq backup-directory-alist `(("" . ,(no-littering-expand-var-file-name "emacs-backup/"))))
(setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Store the custom file in the etc/directory
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

(push (concat user-emacs-directory "lisp/") load-path)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
;;(setq initial-scratch-message nil)

(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 110))

;; Show the absolute file path in the title bar
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function
                                         " %b" ("%b - Dir:  " default-directory)))))))

(use-package all-the-icons)
(use-package all-the-icons-dired :hook (dired-mode . all-the-icons-dired-mode))

(defun onw/set-faces ()
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 120 :weight 'light)
  (set-face-attribute 'variable-pitch nil :family "FiraGO" :height 120 :weight 'light)
  (set-face-attribute 'fill-column-indicator nil :background "white" :foreground "white")
  (set-fontset-font t 'symbol "Noto Color Emoji")

  (defgroup onw-faces nil "Oliver Winspear's personal faces" :group 'faces)
  (defface onw/org-bullets-face
    '((t :font "Symbola" :height 120))
    "Face for org-bullets-mode"
    :group 'onw-faces)

  ;; Make sure the fonts are only set once
  (remove-hook 'server-after-make-frame-hook #'onw/set-faces))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'onw/set-faces)
  (add-hook 'after-init-hook #'onw/set-faces))

(use-package mixed-pitch
  ;;:hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

(use-package good-scroll :config (good-scroll-mode 1))

(use-package doom-modeline :config (doom-modeline-mode 1))

;; Display the column number in the mode line
(column-number-mode 1)

(use-package nyan-mode :config (nyan-mode))

;; Treat all themes as safe
(setq custom-safe-themes t)

(use-package moe-theme
  :init
  (defvar moe-theme-mode-line-color 'yellow)
  :config
  (setq moe-theme-highlight-buffer-id t))
  ;;(moe-dark))

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend)
        modus-themes-fringes nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

;; Make comments more visible
(set-face-foreground 'font-lock-comment-face "pink")

(use-package pulsar
  :config
  (setq pulsar-face 'pulsar-magenta)
  (pulsar-global-mode 1))

;; Pulse the line after jumping somewhere with avy.
(defadvice avy-jump (after avy-pulse-after-jump activate)
  (pulsar-pulse-line))

(setq whitespace-style '(tab-mark))
(global-whitespace-mode)

(use-package hydra)
(use-package major-mode-hydra)

(use-package which-key :config (which-key-mode))

(defun xah/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

(global-set-key (kbd "<f5>") #'xah/new-empty-buffer)

(global-set-key (kbd "s-i") (lambda ()
                              (interactive)
                              (find-file (concat user-emacs-directory "Emacs.org"))))

(pretty-hydra-define hydra-applications (:quit-key "q" :color teal)
  ("Applications" (("l" libera-chat "Connect to Libera Chat with ERC")
                   ("e" elfeed "Elfeed")
                   ("v" vterm-other-window "vterm")
                   ("q" nil "Quit"))))

(bind-key "s-a" #'hydra-applications/body 'global-map)

(use-package erc
  :straight (:type built-in)
  :config
  (setq erc-nick "olnw")
  (setq erc-prompt-for-password nil)
  (setq erc-prompt-for-nickserv-password nil)

  (defun libera-chat ()
        (interactive)
        (erc-tls :server "irc.au.libera.chat"
                 :port   "6697")))

(use-package pdf-tools
  :config
  (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                   ,(face-attribute 'default :background)))

  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (pdf-view-midnight-minor-mode)
    				  (auto-revert-mode)))) ; Display changes live

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-view-restore-mode)

  ;; Save information to a custom location
  (setq pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore")))

(defun prot-common-crm-exclude-selected-p (input)
  "Filter out INPUT from `completing-read-multiple'.
Hide non-destructively the selected entries from the completion
table, thus avoiding the risk of inputting the same match twice.

To be used as the PREDICATE of `completing-read-multiple'."
  (if-let* ((pos (string-match-p crm-separator input))
            (rev-input (reverse input))
            (element (reverse
                      (substring rev-input 0
                                 (string-match-p crm-separator rev-input))))
            (flag t))
      (progn
        (while pos
          (if (string= (substring input 0 pos) element)
              (setq pos nil)
            (setq input (substring input (1+ pos))
                  pos (string-match-p crm-separator input)
                  flag (when pos t))))
        (not flag))
    t))

(defun prot-elfeed-search-tag-filter ()
  "Filter Elfeed search buffer by tags using completion.

Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
  (interactive)
  (unwind-protect
      (elfeed-search-clear-filter)
    (let* ((elfeed-search-filter-active :live)
           (db-tags (elfeed-db-get-all-tags))
           (plus-tags (mapcar (lambda (tag)
                                (format "+%s" tag))
                              db-tags))
           (minus-tags (mapcar (lambda (tag)
                                 (format "-%s" tag))
                               db-tags))
           (all-tags (delete-dups (append plus-tags minus-tags)))
           ;; REQUIRE-MATCH is set to nil to allow arbitrary input
           (tags (completing-read-multiple
                  "Apply one or more tags: "
                  all-tags #'prot-common-crm-exclude-selected-p nil))
           (input (string-join `(,elfeed-search-filter ,@tags) " ")))
      (setq elfeed-search-filter input))
    (elfeed-search-update :force)))

(use-package elfeed
  :config
  ;; Load my feeds from a separate file
  (load "onw-elfeed-feeds.el")

  ;; Customise the default filter
  (elfeed-search-set-filter "+unread")
  (setq elfeed-search-title-max-width 100)

  (defun onw/play-with-mpv ()
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (links (mapcar #'elfeed-entry-link entries)))

      ;; Mark selected entries as unread
      (elfeed-search-untag-all-unread)

      ;; Play all selected entries with mpv
      (cl-loop for link in links
               do (call-process-shell-command (concat "mpv '" link "' \&") nil 0))))

  :bind (:map elfeed-search-mode-map
              ("C-c C-o" . onw/play-with-mpv)
              ("s"       . prot-elfeed-search-tag-filter)))

(use-package notmuch)

(use-package helm
  :preface (require 'helm-config)
  :init (setq helm-command-prefix-key "s-h")
  :config
  ;; Open helm buffer inside current window
  (setq helm-split-window-inside-p t)

  ;; https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
  (defadvice helm-find-files (after find-file-sudo activate)
    "Find file as root if necessary."
    (unless (and buffer-file-name
                 (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (helm-mode 1)
  :bind (("M-x"   . helm-M-x)
         ("s-b"   . helm-bookmarks)
         ("s-f"   . helm-find-files)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i"   . helm-execute-persistent-action)
         ("C-z"   . helm-select-action)))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package avy
  :config (avy-setup-default)
  :bind (("C-:"   . 'avy-goto-char)
         ("C-'"   . 'avy-goto-char-2)
         ("M-g f" . 'avy-goto-line)
         ("M-g w" . 'avy-goto-word-1)
         ("M-g e" . 'avy-goto-word-0)))

(use-package projectile
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package helm-projectile
  :init
  (setq projectile-completion-system 'helm)
  :hook
  (after-init . helm-projectile-on))

(pretty-hydra-define hydra-projectile (:title "Projectile" :quit-key "q" :color teal)
  ("This Frame/Window" (("s"  helm-projectile-switch-project "Switch Projects")
                        ("f" helm-projectile-find-file "Find File In Project")
                        ("n" helm-projectile-find-file-in-known-projects "Find File In All Projects")
                        ("d" helm-projectile-find-dir "Find Dir In Project")
                        ("p" helm-projectile-find-file-dwim "Find File At Point")
                        ("r" helm-projectile-recentf "Find Recent Files")
                        ("b" helm-projectile-switch-to-buffer "Switch Buffers")
                        ("a" helm-projectile-ag "Search Project"))
   "Other Frame/Window" (("F" projectile-find-file-other-frame "Find File Other Frame")
                         ("w" projectile-find-file-other-window "Find File Other Window")
                         ("o" projectile-find-other-file-other-window "Find Other Other Window")
                         ("B" projectile-switch-to-buffer-other-window "Switch Buffer Other Window")
                         ("m" projectile-multi-occur "Search Multi Occurances"))
   "Actions" (("c" projectile-edit-dir-locals "Add Project Config")
              ("I" projectile-invalidate-cache "Clear Projectile Cache")
              ("S" projectile-run-shell "Run Shell")
              ("v" projectile-save-project-buffers "Save Project Buffers")
              ("k" projectile-kill-project-buffers "kill Project Buffers")
              ("t" projectile-toggle-read-only "Toggle Project Read Only")
              ("D" projectile-discover-projects-in-directory "Discover Projects Directory")
              ("q" nil "Quit" :color blue))))

(bind-key "s-p H" 'hydra-projectile/body)

(use-package helm-ag)

(use-package magit)

(setq backward-delete-char-untabify-method 'hungry)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode t)

(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   (sly-mode  . rainbow-delimiters-mode))
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse")))) ; dark red
  (rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "yellow")))) ; black
  (rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
  (whitespace-tab ((t (:foreground "#636363")))))

(setq-default tab-width 8)
(setq-default fill-column 79)
(setq-default indent-tabs-mode t)
(setq-default electric-indent-inhibit t)
(electric-indent-mode -1)

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode       . aggressive-indent-mode)))

(add-hook 'prog-mode-hook (lambda ()
                            (display-fill-column-indicator-mode)
                            (display-line-numbers-mode)))

(setq c-default-style "linux")

(setq inferior-lisp-program "clisp")
(use-package lispy :hook ((lisp-mode       . lispy-mode)
                          (emacs-lisp-mode . lispy-mode)))

(use-package sly :hook (common-lisp-mode . sly-mode))

(defun onw/lisp-setup ()
  (setq indent-tabs-mode nil)
  (setq fill-column 100))

(add-hook 'lisp-mode-hook #'onw/lisp-setup)
(add-hook 'emacs-lisp-mode-hook #'onw/lisp-setup)

(use-package lpy :hook (python-mode . lpy-mode))
(add-hook 'python-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package lsp-mode
  :init
  ;; Set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "s-l")
  :hook ((python-mode . lsp)
         (c-mode      . lsp)
         (c++-mode    . lsp)
         (lsp-mode    . lsp-enable-which-key-integration)))

(use-package lsp-ui)

(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package dap-mode)
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset tab-width))

(setq sgml-basic-offset tab-width)
(setq css-indent-offset tab-width)

;;(load "latexmk-mode.el")
;;(add-hook 'LaTeX-mode-hook #'latexmk-mode)

(use-package org
  :straight (:type built-in)
  :bind (:map global-map
              ("\C-cl" . org-store-link)
              ("\C-ca" . org-agenda)
         :map org-mode-map
              ("C-'"   . nil)) ; I use this binding for avy-goto-char-2

  :config
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t) ; Globally turn on Org Indent mode

  (setq org-directory "/mnt/hdd/org")
  (setq org-agenda-files (list org-directory))

  (push 'org-habit org-modules) ; Add org-habit to the list of modules

  (setq org-hide-leading-stars t)

  ;; Change the colour of the face that's used to hide leading stars
  ;; The value should be #303030 for the moe-dark theme
  (set-face-attribute 'org-hide nil :foreground "#000000")

  (add-hook 'org-mode-hook (lambda ()
                             (setq fill-column 100)
                             (visual-line-mode))))

(with-eval-after-load 'org
  (org-babel-do-load-languages
    'org-babel-load-languages
	'((emacs-lisp . t)
	  (python . t))))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 100))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("☯" "○" "✸" "✿" "~"))
  (setq org-bullets-face-name 'onw/org-bullets-face))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "/mnt/hdd/org-roam")
  ;; Completion without using double square brackets
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
    :target (file+head "${slug}.org.gpg"
                       "#+title: ${title}\n")
    :unnarrowed t)))
  (org-roam-db-autosync-mode))
