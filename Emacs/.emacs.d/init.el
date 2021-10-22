;; https://www.reddit.com/r/emacs/comments/9rrhy8/emacsers_with_beautiful_initel_files_what_about/
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

(setq inferior-lisp-program "sbcl")

(use-package lispy :ensure t)

(use-package sly :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
  (whitespace-tab ((t (:foreground "#636363")))))

(use-package moe-theme
  :ensure t
  :config
  (setq moe-theme-highlight-buffer-id t)
  (moe-dark))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  
  :config
  ;; Move the cursor with j k l ;                             
  (define-key evil-motion-state-map "j" 'evil-backward-char)  
  (define-key evil-motion-state-map "k" 'evil-next-line)      
  (define-key evil-motion-state-map "l" 'evil-previous-line)
  (define-key evil-motion-state-map ";" 'evil-forward-char)   

  ;; Open treemacs with C-n
  (define-key evil-normal-state-map (kbd "C-n") 'treemacs)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package treemacs 
  :after evil
  :ensure t 
  :defer t 
  :config
  (treemacs-filewatch-mode t) 


  :bind (:map global-map
              ("C-x t 1"   . treemacs-delete-other-windows) 
              ("C-x t B"   . treemacs-bookmark) 
              ("C-x t C-t" . treemacs-find-file) 
              ("C-x t M-t" . treemacs-find-tag)
              ("C-n"       . treemacs)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t
  :config
  (evil-define-key 'treemacs treemacs-mode-map (kbd "j") #'treemacs-COLLAPSE-action)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "k") #'treemacs-next-line)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "l") #'treemacs-previous-line)
  (evil-define-key 'treemacs treemacs-mode-map (kbd ";") #'treemacs-RET-action))

(use-package org
  :bind (:map global-map
	      ("\C-cl" . org-store-link)
	      ("\C-ca" . org-agenda))
  :config
  (setq org-log-done t))

;; Easy buffer switching with C-x b
(use-package ido
  :config (ido-mode 1))

;; Open latex-preview-pane... I don't think this can handle bibliographies.
;; (define-key global-map (kbd "C-,") 'latex-preview-pane-mode)
;; Manual way: (define-key global-map (kbd "C-,") (lambda() (interactive) (shell-command (format "pdflatex %s &" (buffer-file-name)))))

;; https://old.reddit.com/r/emacs/comments/k7sx2n/latexpreviewpane_and_latexmk/
(load-file "~/.emacs.d/latexmk-mode.el")
(add-hook 'LaTeX-mode-hook 'latexmk-mode)

;; Set indentation levels style
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character))

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
  
  (when (version<= "26.0.50" emacs-version) 
    (global-display-line-numbers-mode))

  ;; Show cursor position.
  (column-number-mode 1)

  (setq-default word-wrap t)

  ;; Highlight text between parentheses.
  (show-paren-mode t)
  (setq show-paren-style 'expression)

  ;; Backspace tabs properly
  (setq backward-delete-char-untabify-method 'hungry)

  (setq-default tab-width 8)

  (setq-default sgml-basic-offset tab-width)
  (setq-default css-indent-offset tab-width)
  (setq-default evil-shift-width tab-width)


  (setq-default electric-indent-inhibit t)

  ;; Aggressive indent (DEMO: https://github.com/Malabarba/aggressive-indent-mode)
  ;; Maybe enable for prog-mode in the future.
  ;; Indent with tabs for all languages except Lisps.

  (use-package aggressive-indent :ensure t)

  (add-hook 'prog-mode-hook (lambda ()
                              (highlight-indent-guides-mode)
                              (rainbow-delimiters-mode)
                              (setq-default indent-tabs-mode t)))

  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (setq-default indent-tabs-mode nil)
				    (aggressive-indent-mode)))

  (add-hook 'lisp-mode-hook (lambda ()
			      (setq-default indent-tabs-mode nil)
			      (aggressive-indent-mode)))

  (setq c-default-style "linux")

  ;; Might want: https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
  :bind (:map global-map
	      ("C-x c"  . comint-clear-buffer)
	      ("C-x x"  . shell)))

;; https://old.reddit.com/r/emacs/comments/4ew1s8/blurry_pdf_in_pdftools_and_docviewmode/
(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                   ,(face-attribute 'default :background)))
  
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (pdf-view-midnight-minor-mode)
				  (auto-revert-mode)))) ;; Display changes live

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)

  ;; Save information to custom location.
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))
