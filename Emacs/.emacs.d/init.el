;; Minimal Emacs config for Elfeed and Org/Org-roam.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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

;; Load path
(push (concat user-emacs-directory "lisp/") load-path)

(use-package no-littering
  :config
  ;; Store backup and auto-save files in the var/ directory
  (setq backup-directory-alist `(("" . ,(no-littering-expand-var-file-name "emacs-backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; Store the custom file in the etc/directory
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Key bindings
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

;; Scrolling
(use-package emacs :config (setq scroll-conservatively 1000))

;; Completions
(use-package emacs
  :config
  (icomplete-vertical-mode 1)
  (savehist-mode 1)
  ;; (push 'flex completion-styles)
  (setq tab-always-indent 'complete))

(use-package orderless
  :init
  ;; partial-completion allows multiple files to be opened at once
  ;; with find-file, if a wildcard is entered.
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Visual
(use-package emacs
  :init
  ;; Add customisations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  (setq custom-safe-themes t) ; Treat all themes as safe
  (setq show-paren-delay 0)
  (setq inhibit-startup-screen t)
  (column-number-mode 1)
  :config (load-theme 'modus-vivendi))

;; RSS
(use-package elfeed
  :config
  ;; Load my feeds from a separate file
  (load "olnw-elfeed-feeds.el")
  
  ;; Customise the default filter
  (elfeed-search-set-filter "+unread")
  (setq elfeed-search-title-max-width 100)
  
  (defun olnw/play-with-mpv ()
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (links (mapcar #'elfeed-entry-link entries)))
      
      ;; Mark selected entries as read
      (elfeed-search-untag-all-unread)
      
      ;; Play all selected entries with mpv
      (cl-loop for link in links
               do (call-process-shell-command (concat "mpv '" link "' \&") nil 0))))
  
  :bind (:map elfeed-search-mode-map
              ("C-c C-o" . olnw/play-with-mpv)))

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

  (setq org-directory "/mnt/hdd/org")

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
  (org-roam-directory "/mnt/hdd/org-roam")
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

