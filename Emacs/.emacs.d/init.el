;; Use 100MB of consing between garbage collections
(setq gc-cons-threshold (* 100 1000 1000))

(add-hook 'after-init-hook (lambda ()
                             ;; Restore value after startup
                             (setq gc-cons-threshold 800000)))

(set-default-coding-systems 'utf-8)

(setq user-emacs-directory "~/stow/Emacs/.emacs.d/")

(push (concat user-emacs-directory "lisp/") load-path)

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

(use-package no-littering)

;; Store backup and auto-save files in the var/ directory
(setq backup-directory-alist `(("" . ,(no-littering-expand-var-file-name "emacs-backup/"))))
(setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Store the custom file in the etc/directory
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

(defun olnw/systemd-restart-emacs ()
  (interactive)
  (save-some-buffers)
  (shell-command "systemctl --user restart emacs"))

(defun olnw/systemd-stop-emacs ()
  (interactive)
  (save-some-buffers)
  (shell-command "systemctl --user stop emacs"))

(use-package moe-theme
  ;;:init
  ;;(defvar moe-theme-mode-line-color 'yellow)
  :config
  (setq moe-theme-highlight-buffer-id t))

(use-package emacs
  :init
  ;; Add customisations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Treat all themes as safe
  (setq custom-safe-themes t))

;; Make comments more visible
;;(set-face-foreground 'font-lock-comment-face "pink")

;; Packaged version of the Modus themes, for older Emacs versions
;; that don't have them installed by default.
(use-package modus-themes
  :init (modus-themes-load-themes)
  :config (modus-themes-load-vivendi))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(defun olnw/set-faces ()
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 120 :weight 'light)
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 120 :weight 'light)
  (set-face-attribute 'variable-pitch nil :family "FiraGO" :height 120 :weight 'light)
  (set-face-attribute 'fill-column-indicator nil :background "white" :foreground "white")
  (set-fontset-font t 'symbol "Noto Color Emoji")

  (defgroup olnw-faces nil "Faces created by Oliver Winspear" :group 'faces)
  (defface olnw/org-bullets-face
    '((t :font "Symbola" :height 120))
    "Face for org-bullets-mode"
    :group 'olnw-faces)

  ;; Make sure the faces are only set once
  (remove-hook 'server-after-make-frame-hook #'olnw/set-faces))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'olnw/set-faces)
  (add-hook 'after-init-hook #'olnw/set-faces))

(use-package mixed-pitch
  ;;:hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t))

(use-package all-the-icons)
(use-package all-the-icons-dired :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline :config (doom-modeline-mode 1))

;; Display the column number in the mode line
(column-number-mode 1)

;; Display Nyan Cat in the modeline
;; This is necessary; trust me.
(use-package nyan-mode :config (nyan-mode))

(setq whitespace-style '(tab-mark))
(global-whitespace-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package pulsar
  :defer 3
  :config
  (setq pulsar-face 'pulsar-magenta)
  (pulsar-global-mode 1))

;; Pulse the line after jumping somewhere with avy.
(defadvice avy-action-goto (after avy-pulse-after-goto activate)
  (pulsar-pulse-line))

(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   (sly-mrepl . rainbow-delimiters-mode))
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

(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode 1)

(use-package typo :defer 3)

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

(use-package meow
  :config
  (setq meow-expand-hint-remove-delay 100)

  (defun meow-setup ()
    ;; No cheatsheet for Colemak-DH :(
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
    (meow-motion-overwrite-define-key
     '("<escape>" . ignore)
     '("e" . meow-next)
     '("i" . meow-prev))
    (meow-leader-define-key
     '("?" . meow-cheatsheet)
     ;; To execute the originally e in MOTION state, use SPC e.
     '("e" . "H-e")
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument))
    (meow-normal-define-key
     '("`" . ace-window)
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("/" . meow-visit)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("e" . meow-next)
     '("E" . meow-next-expand)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . execute-extended-command)
     '("H" . repeat)
     '("i" . meow-prev)
     '("I" . meow-prev-expand)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-left)
     '("N" . meow-left-expand)
     '("o" . meow-right)
     '("O" . meow-right-expand)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("s" . meow-insert)
     '("S" . meow-open-above)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-search)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-delete)
     '("X" . meow-backward-delete)
     '("y" . meow-save)
     '("z" . meow-pop-selection)
     '("'" . meow-block)
     '("\"" . meow-to-block)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode))

;; Minimalistic minibuffer completion UI
(use-package vertico :init (vertico-mode))

;; Persist history over Emacs restarts.
;; Vertico sorts by history position.
(use-package savehist
  :straight nil
  :init (savehist-mode))

;; Add marginalia to minibuffer completions
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; 'orderless' is a completion style that can match multiple
;; space-separated components in any order
(use-package orderless
  :init
  ;; partial-completion allows multiple files to be opened at once
  ;; with find-file, if a wildcard is entered.
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Additional completion-at-point functions
(use-package cape
  ;; Ensure that orderless comes first in completion-styles
  :after orderless
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Bookmarks, buffer-switching, searching, grep...
(use-package consult
  ;; Replace bindings. Lazily loaded by 'use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
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
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Configure the register formatting. This improves the register
  ;; preview for 'consult-register', 'consult-register-load',
  ;; 'consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Replace 'completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  )

(use-package consult-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; Menu that provides context-specific actions
(use-package embark
  :bind
  ("C-."   . embark-act)
  ("C-;"   . embark-dwim)      ;; Good alternative: M-.
  ("C-h B" . embark-bindings)  ;; Alternative for 'describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Integration for embark and consult.
;; 'It provides exporters for several Consult commands and also
;; tweaks the behavior of many Consult commands when used as actions
;; with embark-act in subtle ways that you may not even notice, but
;; make for a smoother experience.'
(use-package embark-consult
  :after (embark consult)
  :demand t ; Only necessary if you have the hook below
  ;; If you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; In-buffer completions (similar to company)
(use-package corfu
  :init (global-corfu-mode)
  :config
  ;; This means Corfu will be used for completions when running
  ;; M-: (eval-expression). Vertico doesn't support completions
  ;; for this.
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
            (bound-and-true-p vertico--input))
  ;; (setq-local corfu-auto nil) Enable/disable auto completion
  (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to 'completing-read-multiple'.
  ;; Alternatively try 'consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Disable case-sensitivity for file and buffer matching
  ;; with built-in completion styles.
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent 'complete))

(use-package ace-window)

(use-package avy
  :config (avy-setup-default)
  :bind
  ("H-:"   . 'avy-goto-char)
  ("H-'"   . 'avy-goto-char-2)
  ("H-g f" . 'avy-goto-line)
  ("H-g w" . 'avy-goto-word-1)
  ("H-g e" . 'avy-goto-word-0))

(use-package projectile
  :defer 3
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "H-p") 'projectile-command-map))

(setq auto-window-vscroll nil) ; Potentially fixes jumpy scrolling
(setq scroll-conservatively 1000) ; Don't recenter the point if it moves off screen
(setq mouse-wheel-scroll-amount '(4)) ; Scroll four lines at a time with the mouse wheel
(setq mouse-wheel-progressive-speed nil) ; Non-accelerated mouse wheel scrolling

;;(use-package good-scroll :config (good-scroll-mode 1))

(pretty-hydra-define hydra-applications (:quit-key "q" :color teal)
  ("Applications" (("l" libera-chat "Connect to Libera Chat with ERC")
                   ("e" elfeed "Elfeed")
                   ("v" vterm-other-window "vterm")
                   ("q" nil "Quit"))))

(global-set-key (kbd "H-a") 'hydra-applications/body)

(use-package erc
  :defer 3
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
  :defer 3
  :config
  (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                   ,(face-attribute 'default :background)))

  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (pdf-view-midnight-minor-mode)
                                  (auto-revert-mode)))) ; Display changes live

(use-package pdf-view-restore
  :defer 3
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
  :defer 3
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
              ("C-c C-o" . olnw/play-with-mpv)
              ("s"       . prot-elfeed-search-tag-filter)))

(use-package vterm :defer 3)

(setq backward-delete-char-untabify-method 'hungry)

;; Display tab characters with a max width of 8 columns.
;; This is unaffected by indent-tabs-mode.
(setq-default tab-width 8)

;; By default, don't allow indentation to insert tabs.
(setq-default indent-tabs-mode nil)

;; Auto-indentation
(electric-indent-mode 1)

;; Disable in Org mode. It was behaving weirdly for me.
(add-hook 'org-mode-hook
          (lambda () (electric-indent-local-mode -1)))

;; Indent with tabs for HTML and CSS
(setq sgml-basic-offset tab-width)
(setq css-indent-offset tab-width)
(add-hook 'html-mode-hook (lambda () (setq indent-tabs-mode t)))
(add-hook 'css-mode-hook (lambda () (setq indent-tabs-mode t)))

;; [[https://stackoverflow.com/questions/39894233/extract-emacs-c-style-options-from-clang-format-style][Getting CC mode style options from a clang-format file]]:
;; https://stackoverflow.com/q/39894233

;; Set the indentation style for CC mode.
(setq c-default-style
  '((java-mode . "java")
    (awk-mode  . "awk")
    (other     . "gnu")))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode       . aggressive-indent-mode)))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; Doesn't seem to be working, even though org-eldoc-documentation-function
;; is added to eldoc-documentation-functions when in Org mode. I'll just
;; leave this here until it starts working or I know how to fix it.
(use-package org-contrib)
(require 'org-eldoc)

(use-package magit :defer 3)

(setq inferior-lisp-program "ros -Q run")

(use-package paredit
  :hook ((emacs-lisp-mode                  . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode                        . enable-paredit-mode)
         (lisp-mode                        . enable-paredit-mode)
         (lisp-interaction-mode            . enable-paredit-mode)
         (scheme-mode                      . enable-paredit-mode)
         (sly-mrepl                        . enable-paredit-mode)))

(use-package sly
  :config
  (require 'sly-autoloads)
  (setq sly-contribs '(sly-mrepl))
  (sly-setup)

  ;; Disable Sly's completion UI. I use Corfu instead.
  (sly-symbol-completion-mode -1))

(defun olnw/lisp-setup ()
  (setq fill-column 100))

(add-hook 'lisp-mode-hook #'olnw/lisp-setup)
(add-hook 'emacs-lisp-mode-hook #'olnw/lisp-setup)

(use-package lsp-mode
  :init
  ;; Set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "s-l")
  :hook ((python-mode . lsp)
         (c-mode      . lsp)
         (c++-mode    . lsp)
         (lsp-mode    . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map ("<tab>" . indent-for-tab-command)))

(use-package lsp-ui)

(use-package dap-mode
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

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
  (setq org-log-done t)

  (setq org-directory "/mnt/hdd/org")

  ;; I can’t figure out how to match either .org or .org.gpg files
  ;; Fix this in the future
  (setq org-agenda-file-regexp "\\`[^.].*\\.org.gpg\\'")
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
  (setq org-bullets-face-name 'olnw/org-bullets-face))

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
  (org-roam-db-autosync-mode))

;; 'Collection of functions to operate org-roam with the help of
;; consult and its live preview feature.'
(use-package consult-org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor-mode
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ("C-c n f" . org-roam-node-find) ; Can't create new nodes with consult-org-roam-file-find?
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n s" . consult-org-roam-search)
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n i" . org-roam-node-insert))
