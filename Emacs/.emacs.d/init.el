;; TODO:
;; - Revamp all key bindings
;;     - Change helm prefix key

;; - Literate config in Org file

;; - Create a better environment for writing prose

;; Key bindings:

;; TREEMACS

;; C-x t 1    treemacs-delete-other-windows
;; C-x t B    treemacs-bookmark
;; C-x t C-t  treemacs-find-file
;; C-x t M-t  treemacs-find-tag
;; C-n        treemacs

;; ORG

;; \C-cl  org-store-link
;; \C-ca  org-agenda

;; C-c n l  org-roam-buffer-toggle
;; C-c n f  org-roam-node-find
;; C-c n i  org-roam-node-insert

;; HELM

;; M-x  helm-M-x
;; s-b  helm-buffers-list
;; s-f  helm-find-files
;; s-s  helm-occur-from-isearch

;; helm-map

;; <tab>  helm-execute-persistent-action
;; C-i    helm-execute-persistent-action
;; C-z    helm-select-action

;; https://www.reddit.com/r/emacs/comments/9rrhy8/emacsers_with_beautiful_initel_files_what_about/
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Don't back up files inside of the working directory.
(setq backup-directory-alist `(("" . ,(concat user-emacs-directory "emacs-backup"))))

;; Set up straight.el
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

(setq inferior-lisp-program "clisp")

(use-package lispy)

(use-package sly
  :config
  (add-hook 'sly-mode-hook #'rainbow-delimiters-mode))

;; Custom colours for parentheses
(use-package rainbow-delimiters
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

(use-package moe-theme
  :init
  ;; (defvar moe-theme-mode-line-color 'yellow)
  :config
  (setq moe-theme-highlight-buffer-id t)
  (moe-light))

;; Make comments more visible
;;(set-face-foreground 'font-lock-comment-face "pink")

;; Nyan cat in modeline
(use-package nyan-mode
  :config
  (nyan-mode))

(use-package evil
  :init
  (setq evil-want-integration t)        ; This is optional since it's already set to t by default
  (setq evil-want-keybinding nil)
  
  :config
  ;; Move the cursor with j k l ;                             
  ;; (define-key evil-motion-state-map "j" 'evil-backward-char)  
  ;; (define-key evil-motion-state-map "k" 'evil-next-line)      
  ;; (define-key evil-motion-state-map "l" 'evil-previous-line)
  ;; (define-key evil-motion-state-map ";" 'evil-forward-char)   

  ;; Open treemacs with C-n
  (define-key evil-normal-state-map (kbd "C-n") 'treemacs)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (setq evil-want-fine-undo t))

(use-package treemacs 
  :after evil
  :defer t 
  :config
  (treemacs-filewatch-mode t) 

  :bind (:map global-map
              ("C-x t 1"   . treemacs-delete-other-windows) 
              ("C-x t B"   . treemacs-bookmark) 
              ("C-x t C-t" . treemacs-find-file) 
              ("C-x t M-t" . treemacs-find-tag)
              ("C-n"       . treemacs)))

(use-package treemacs-evil :after (treemacs evil))
;; :config
;; (evil-define-key 'treemacs treemacs-mode-map (kbd "j") #'treemacs-COLLAPSE-action)
;; (evil-define-key 'treemacs treemacs-mode-map (kbd "k") #'treemacs-next-line)
;; (evil-define-key 'treemacs treemacs-mode-map (kbd "l") #'treemacs-previous-line)
;; (evil-define-key 'treemacs treemacs-mode-map (kbd ";") #'treemacs-RET-action))

;; Automatically chooses what text to display as variable-pitch and fixed-pitch
(use-package mixed-pitch)

;; Wrap text at fill-column instead of at the window edge,
;; in buffers that use visual-line-mode.
(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package org
  :bind (:map global-map
	      ("\C-cl" . org-store-link)
	      ("\C-ca" . org-agenda))
  :config
  (setq org-log-done t)

  ;; Computer Modern
  ;; https://sourceforge.net/projects/cm-unicode/
  ;; (set-face-attribute 'variable-pitch nil :font "CMU Serif" :height 200) ; :height may not work
  (add-hook 'org-mode-hook (lambda ()
                             (mixed-pitch-mode)
                             (setq fill-column 70) ; Better readability
                             (visual-line-mode)
                             (setq org-hide-emphasis-markers t))))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "/mnt/hdd/Documents/org-roam")
  ;; Completion without using double square brackets
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package helm
  :preface (require 'helm-config)
  :config
  ;; Open helm buffer inside current window
  (setq helm-split-window-in-side-p t)

  ;; https://emacsredux.com/blog/2013/04/21/edit-files-as-root/
  (defadvice helm-find-files (after find-file-sudo activate)
    "Find file as root if necessary."
    (unless (and buffer-file-name
                 (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (helm-mode 1)
  :bind (("M-x"   . helm-M-x)
         ("s-b"   . (lambda () (interactive) (helm-buffers-list)))
         ("s-f"   . helm-find-files)
         ("s-s"   . helm-occur-from-isearch)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i"   . helm-execute-persistent-action)
         ("C-z"   . helm-select-action)))

;; Requires The Silver Searcher to be installed
(use-package helm-ag)

;; Invoke pdflatex manually 
;; (define-key global-map (kbd "C-,") (lambda() (interactive)                                    
;;                                      (shell-command                                            
;;                                        (format "pdflatex %s &" (buffer-file-name)))))

;; https://old.reddit.com/r/emacs/comments/k7sx2n/latexpreviewpane_and_latexmk/
(load-file "~/.emacs.d/latexmk-mode.el")
(add-hook 'LaTeX-mode-hook #'latexmk-mode)

;; Set indentation levels style
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character))

;; Aggressive indent (DEMO: https://github.com/Malabarba/aggressive-indent-mode)
;; Maybe enable for prog-mode in the future
(use-package aggressive-indent)

(use-package emacs
  :config
  ;; Custom directory for autosave files
  (setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t)))
  
  ;; Show file full path in title bar
  (setq-default frame-title-format
                (list '((buffer-file-name " %f"
                                          (dired-directory
                                           dired-directory
                                           (revert-buffer-function
                                            " %b" ("%b - Dir:  " default-directory)))))))
  
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (setq initial-buffer-choice "/mnt/hdd/Documents/start.org")
  
  (when (version<= "26.0.50" emacs-version) 
    (global-display-line-numbers-mode))

  ;; Set the default window size
  (add-to-list 'default-frame-alist '(height . 35))
  (add-to-list 'default-frame-alist '(width . 110))

  ;; Display the column number in the mode line
  (column-number-mode 1)

  ;; Highlight text between parentheses
  (setq show-paren-delay 0)
  (show-paren-mode t)
  (setq show-paren-style 'expression)

  ;; Backspace tabs properly
  (setq backward-delete-char-untabify-method 'hungry)

  (setq-default tab-width 8)

  (setq-default sgml-basic-offset tab-width)
  (setq-default css-indent-offset tab-width)
  (setq-default evil-shift-width tab-width)

  (setq-default electric-indent-inhibit t)

  (add-hook 'prog-mode-hook (lambda ()
                              (highlight-indent-guides-mode)
                              (rainbow-delimiters-mode)
                              (setq-default indent-tabs-mode t)
                              (display-fill-column-indicator-mode)))

  (defun my-lisp-mode-hook ()
    (setq-default indent-tabs-mode nil)
    (aggressive-indent-mode)
    (setq-default fill-column 100))

  (add-hook 'emacs-lisp-mode-hook #'my-lisp-mode-hook)

  (add-hook 'lisp-mode-hook #'my-lisp-mode-hook)

  (setq c-default-style "linux")

  ;; Might want: https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop

  :bind (:map global-map
	      ("s-t"  . vterm-other-window)
              ("s-i"  . (lambda () (interactive) (switch-to-buffer
                                                  (find-file-noselect "~/.emacs.d/init.el"))))))
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'web-mode-hook (lambda () (setq web-mode-markup-indent-offset sgml-basic-offset))))

;; Run M-x pdf-tools-install
;; https://old.reddit.com/r/emacs/comments/4ew1s8/blurry_pdf_in_pdftools_and_docviewmode/
(use-package pdf-tools
  :config
  (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                   ,(face-attribute 'default :background)))
  
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (pdf-view-midnight-minor-mode)
  				  (auto-revert-mode)))) ;; Display changes live

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook #'pdf-view-restore-mode)
  
  ;; Save information to custom location
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

;; Run M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline :config (doom-modeline-mode 1))

(use-package vterm)

;; IRC client
(use-package erc
  :config
  (setq erc-nick "Basspoon")

  (defun libera-chat ()
    (interactive)
    (erc-tls :server "irc.au.libera.chat"
             :port   "6697")))

;; Git integration
(use-package magit)

;; Smooth scrolling
(use-package good-scroll :config (good-scroll-mode 1))
