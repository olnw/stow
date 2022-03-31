;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)))

(set-default-coding-systems 'utf-8)

(setq load-path (cons (concat user-emacs-directory "lisp") load-path))

;; https://www.reddit.com/r/emacs/comments/9rrhy8/emacsers_with_beautiful_initel_files_what_about/
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq backup-directory-alist `(("" . ,(concat user-emacs-directory "emacs-backup/"))))
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t)))

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
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i"   . helm-execute-persistent-action)
         ("C-z"   . helm-select-action)))

;; Requires The Silver Searcher to be installed
(use-package helm-ag)

(use-package magit)

(use-package erc
:config
(setq erc-nick "Basspoon")
(defun libera-chat ()
      (interactive)
      (erc-tls :server "irc.au.libera.chat"
               :port   "6697")))

(define-key global-map (kbd "s-j") #'backward-char)
(define-key global-map (kbd "s-k") #'next-line)
(define-key global-map (kbd "s-l") #'previous-line)
(define-key global-map (kbd "s-;") #'forward-char)

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
                              (switch-to-buffer (find-file-noselect (concat user-emacs-directory "Emacs.org")))))

(load "latexmk-mode.el")
(add-hook 'LaTeX-mode-hook #'latexmk-mode)

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

(use-package org
  :bind (:map global-map
              ("\C-cl" . org-store-link)
              ("\C-ca" . org-agenda))

  :config
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t) ; Globally turn on Org Indent mode

  (setq org-directory "/mnt/hdd/Documents")

  (setq org-agenda-files '("habits.org"
                           "projects.org"
                           "todo.org"))

  (push 'org-habit org-modules) ; Add org-habit to the list of modules

  (setq org-hide-leading-stars t)

  ;; Change the colour of the face that's used to hide leading stars
  ;; The value should be #303030 for the moe-dark theme
  (set-face-attribute 'org-hide nil :foreground "#000000")

  (add-hook 'org-mode-hook #'visual-line-mode))

;; Automatically tangle our Emacs.org config file when we save it
;; From https://github.com/daviwil/emacs-from-scratch
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t))))

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
  (org-roam-directory "/mnt/hdd/Documents/org-roam")
  ;; Completion without using double square brackets
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

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

(setq-default tab-width 8)
(setq-default fill-column 79)
(setq-default indent-tabs-mode t)
(setq-default electric-indent-inhibit t)

;; Backspace tabs properly
(setq backward-delete-char-untabify-method 'hungry)

;; Highlight text between parentheses
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode t)

(use-package aggressive-indent)

(add-hook 'prog-mode-hook (lambda ()
                            (display-fill-column-indicator-mode)
                            (display-line-numbers-mode)))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

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

(setq c-default-style "linux")

(setq inferior-lisp-program "clisp")
(use-package lispy)
(use-package sly)
(add-hook 'lisp-mode-hook (lambda ()
                            (setq indent-tabs-mode nil)
                            (setq fill-column 100)
                            (aggressive-indent-mode)))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (setq indent-tabs-mode nil)
                                (anaconda-mode)
                                (anaconda-eldoc-mode))))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset tab-width))

(setq sgml-basic-offset tab-width)
(setq css-indent-offset tab-width)

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

(use-package vterm
  :bind (("s-t" . vterm-other-window)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show the absolute file path in the title bar
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function
                                         " %b" ("%b - Dir:  " default-directory)))))))

(setq inhibit-startup-screen t)
;;(setq initial-scratch-message nil)

;; Set the default frame dimensions
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 110))

;; Display the column number in the mode line
(column-number-mode 1)

(use-package all-the-icons)
(use-package all-the-icons-dired :hook (dired-mode . all-the-icons-dired-mode))

(defun onw/set-fonts ()
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 100 :weight 'light)
  (set-face-attribute 'variable-pitch nil :family "FiraGO" :height 100 :weight 'light)
  (set-fontset-font t 'symbol "Noto Color Emoji")

  (defface onw/org-bullets-face '((t :font "Symbola" :height 150)) "Face for org-bullets-mode")

  (remove-hook 'server-after-make-frame-hook #'onw/set-fonts)) ; Make sure the fonts are only set once

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'onw/set-fonts)
  (onw/set-fonts))

(use-package good-scroll :config (good-scroll-mode 1))

(use-package doom-modeline :config (doom-modeline-mode 1))

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
;;(set-face-foreground 'font-lock-comment-face "pink")
