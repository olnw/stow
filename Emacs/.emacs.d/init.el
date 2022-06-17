;; -*- lexical-binding: t -*-
;;;; 'Minimal' GNU Emacs configuration

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(set-default-coding-systems 'utf-8)

(push (concat user-emacs-directory "lisp/") load-path)

(use-package no-littering
  :config
  ;; Store backup and auto-save files in the var/ directory
  (setq backup-directory-alist `(("" . ,(no-littering-expand-var-file-name "emacs-backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; Store the custom file in the etc/directory
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;;; Indentation ;;;

;; By default, don't allow indentation to insert tab characters.
(setq-default indent-tabs-mode nil)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode  . "awk")
        (other     . "gnu")))

;;; Key bindings ;;;

(use-package which-key :init (which-key-mode))

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

(use-package emacs
  :bind
  ("<f5>" . xah/new-empty-buffer)
  ("<f6>" . kill-buffer)
  ("<f7>" . delete-window)
  ("<f8>" . delete-other-windows)
  ("<f9>" . other-window))

;;; Scrolling ;;;

(setq scroll-conservatively 1000)

;;; Completions ;;;

(use-package emacs
  :init
  (savehist-mode 1)
  (setq tab-always-indent 'complete)
  
  ;; Disable case-sensitivity for file and buffer matching
  ;; with built-in completion styles.
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

(use-package orderless
  :init
  ;; partial-completion allows multiple files to be opened at once
  ;; with find-file, if a wildcard is entered.
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package vertico :init (vertico-mode))

(use-package marginalia :init (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded by use-package.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ; orig. yank-pop
         ("<help> a" . consult-apropos)            ; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ; orig. goto-line
         ("M-g o" . consult-outline)               ; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ; orig. next-matching-history-element
         ("M-r" . consult-history))                ; orig. previous-matching-history-element

  ;; The :init configuration is always executed. (Not lazy.)
  :init
  ;; Use 'consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default 'completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Configure the register formatting. This improves the register
  ;; preview for 'consult-register', 'consult-register-load',
  ;; 'consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;; Visual ;;;

;; Highlight matching parens
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression))

;; Reduce UI clutter
(use-package emacs
  :init
  (setq inhibit-startup-screen t)
  :config
  (column-number-mode 1)
  (tab-bar-mode 1)
  (tool-bar-mode -1))

;; Marker for tab characters
(use-package whitespace
   :ensure nil
   :init
   (setq whitespace-style '(tab-mark))
   :config
   (global-whitespace-mode 1))

;; Themes
(use-package emacs
  :init
  ;; Add customisations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  (setq custom-safe-themes t) ; Treat all themes as safe
  :config
  (load-theme 'modus-operandi))

;; Custom faces
(defun olnw/set-faces ()
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 120 :weight 'light)
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 120 :weight 'light)
  (set-face-attribute 'variable-pitch nil :family "FiraGO" :height 120 :weight 'light)

  ;; Make sure the faces are only set once
  (remove-hook 'server-after-make-frame-hook #'olnw/set-faces))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'olnw/set-faces)
  (add-hook 'after-init-hook #'olnw/set-faces))

;;; Org mode ;;;

(use-package org
  :ensure nil ; Use the built-in version of Org mode
  :hook (org-mode . flyspell-mode)
  :bind
  ("C-c a" . org-agenda)
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t) ; Globally turn on Org Indent mode
  (setq org-log-done t)
  (setq org-imenu-depth 1000)
  (setq org-directory "E:\\org")
  (setq org-habit-show-done-always-green t)
  ;; (setq org-habit-show-all-today t)

  ;; I can’t figure out how to match either .org or .org.gpg files
  ;; Fix this in the future
  (setq org-agenda-file-regexp "\\`[^.].*\\.org.gpg\\'")
  (setq org-agenda-files (list org-directory))

  (push 'org-habit org-modules) ; Add org-habit to the list of modules

  (add-hook 'org-mode-hook (lambda ()
                             (setq fill-column 100)
                             (visual-line-mode)))

  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (lisp       . t)
       (python     . t))))

  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh"   . "src sh"))
  (add-to-list 'org-structure-template-alist '("el"   . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc"   . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts"   . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py"   . "src python"))
  (add-to-list 'org-structure-template-alist '("go"   . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "E:\\Org-roam")
  ;; Completion without using double square brackets
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :target (file+head "${slug}.org.gpg"
                                                         "#+title: ${title}\n")
                                      :unnarrowed t)))
  (org-roam-db-autosync-mode)
  :bind
  ("C-c n f" . org-roam-node-find)
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n d" . org-roam-dailies-goto-today))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 100))

;; I use this for incrementing habit streaks
(use-package org-edna :config (org-edna-mode 1))

;; Displaying habit streaks in org agenda
(use-package org-agenda-property
  :config
  (setq org-agenda-property-list '("STREAK")))

;;; Lisp programming ;;;

(setq inferior-lisp-program "sbcl")

(use-package paredit
  :hook ((emacs-lisp-mode                  . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode                        . enable-paredit-mode)
         (lisp-mode                        . enable-paredit-mode)
         (lisp-interaction-mode            . enable-paredit-mode)
         (scheme-mode                      . enable-paredit-mode)
         (sly-mrepl                        . enable-paredit-mode))

  ;; Re-map paredit-splice-sexp from M-s to C-c s
  ;; The default conflicts with Consult's M-s bindings
  :bind (:map paredit-mode-map
              ("M-s" . nil)
              ("C-c s" . paredit-splice-sexp)))

(use-package sly
  :config
  ;; Disable Sly's completion UI.
  (sly-symbol-completion-mode -1))

;; Common Lisp HyperSpec (CLHS) interface from within Emacs.
(use-package clhs
  :custom
  (tags-apropos-additional-actions '(("Common Lisp" clhs-doc clhs-symbols)))
  :bind
  ("C-c d" . clhs-doc))

(use-package rainbow-delimiters
  :hook
  ((eval-expression-minibuffer-setup . rainbow-delimiters-mode)
   (ielm-mode                        . rainbow-delimiters-mode)
   (prog-mode                        . rainbow-delimiters-mode)
   (sly-mrepl                        . rainbow-delimiters-mode))
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "dark red")))) ; chartreuse
  (rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "black")))) ; yellow
  (rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
  (whitespace-tab ((t (:foreground "#636363")))))

;;; LSP and DAP ;;;

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-=")
  :config
  (define-key lsp-mode-map (kbd "C-=") lsp-command-map)
  (define-key lsp-mode-map (kbd "<tab>") #'indent-for-tab-command)

  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'cmake-mode-hook #'lsp)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package lsp-ui)

(use-package dap-mode
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))
