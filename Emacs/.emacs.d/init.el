(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(nov auctex latex-preview-pane evil-collection highlight-indent-guides aggressive-indent elisp-format smart-hungry-delete moe-theme highlight-parentheses slime rainbow-delimiters treemacs-evil treemacs evil)))

(require 'elisp-format)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
 '(whitespace-tab ((t (:foreground "#636363")))))

(load-theme 'moe-dark t)
(show-paren-mode t)
(setq show-paren-style 'expression)

(require 'slime)
(setq inferior-lisp-program "sbcl")

(require 'use-package)
;; https://github.com/emacs-evil/evil-collection
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(define-key evil-motion-state-map "j" 'evil-backward-char)
(define-key evil-motion-state-map ";" 'evil-forward-char)
(define-key evil-motion-state-map "k" 'evil-next-line)
(define-key evil-motion-state-map "l" 'evil-previous-line)

;; Open treemacs with C-n
(define-key evil-normal-state-map (kbd "C-n") 'treemacs)

(use-package treemacs 
  :ensure t 
  :defer t 
  :config (treemacs-filewatch-mode t) 
  :bind (:map global-map
              ("C-x t 1"   . treemacs-delete-other-windows) 
              ("C-x t B"   . treemacs-bookmark) 
              ("C-x t C-t" . treemacs-find-file) 
              ("C-x t M-t" . treemacs-find-tag)))

;; https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Easy buffer switching with C-x b
(ido-mode 1)

;; Open latex-preview-pane... I don't think this can handle bibliographies.
;; (define-key global-map (kbd "C-,") 'latex-preview-pane-mode)
;; Manual way: (define-key global-map (kbd "C-,") (lambda() (interactive) (shell-command (format "pdflatex %s &" (buffer-file-name)))))

;; https://old.reddit.com/r/emacs/comments/k7sx2n/latexpreviewpane_and_latexmk/
(load-file "~/.emacs.d/latexmk-mode.el")
(add-hook 'LaTeX-mode-hook 'latexmk-mode)

;; Clear shell buffer
(define-key global-map (kbd "C-x c") 'comint-clear-buffer)

;; Open shell
(define-key global-map (kbd "C-x x") 'shell)

;; Set indentation levels style
(setq highlight-indent-guides-method 'character)

;;;;;; FORMATTING ;;;;;;
(setq-default word-wrap t)

;; Backspace tabs properly
(setq backward-delete-char-method 'hungry)

(setq-default electric-indent-inhibit t)
(local-set-key (kbd "TAB") 'tab-to-tab-stop) 

;; Indent with tabs for all languages except Lisps
(setq-default tab-width 8)

(add-hook 'prog-mode-hook (lambda () 
                            (highlight-indent-guides-mode) 
                            (rainbow-delimiters-mode)
                            (setq-default indent-tabs-mode t)))

;; Are these necessary?
(add-hook 'lisp-mode-hook (setq-default indent-tabs-mode nil)) 
(add-hook 'python-mode-hook (setq-default indent-tabs-mode nil)) 

(setq-default sgml-basic-offset tab-width)
(setq-default css-indent-offset tab-width)
(setq-default evil-shift-width tab-width)

(setq c-default-style "linux")

;;Aggressive indent (DEMO: https://github.com/Malabarba/aggressive-indent-mode)
;;(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
;;(add-hook 'css-mode-hook #'aggressive-indent-mode)

;; https://stackoverflow.com/questions/1450169/how-do-i-emulate-vims-softtabstop-in-emacs
(local-set-key (kbd "DEL") 'backward-delete-whtiespace-to-column)
(local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
(defun backward-delete-whitespace-to-column () 
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible" 
  (interactive) 
  (if indent-tabs-mode (call-interactively 'backward-delete-char) 
    (let ((movement (% (current-column) tab-width)) 
          (p (point))) 
      (when (= movement 0) 
        (setq movement tab-width)) 
      (save-match-data (if (string-match "\\w*\\(\\s-+\\)$" 
                                         (buffer-substring-no-properties 
                                          (- p movement)
                                          p)) 
                           (backward-delete-char (- (match-end 1) 
                                                    (match-beginning 1))) 
                         (call-interactively 'backward-delete-char))))))

;;;;;; MISC (move more stuff here) ;;;;;;

;; Custom directory for autosave files
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; Show file full path in title bar
(setq-default frame-title-format (list '((buffer-file-name " %f" (dired-directory dired-directory
                                                                                  (revert-buffer-function
                                                                                   " %b" ("%b - Dir:  "
                                                                                          default-directory)))))))

(setq inhibit-startup-screen t)
;;(setq initial-buffer-choice "")

(when (version<= "26.0.50" emacs-version) 
  (global-display-line-numbers-mode))

;; Use pdf-view to view PDFs
;; https://old.reddit.com/r/emacs/comments/4ew1s8/blurry_pdf_in_pdftools_and_docviewmode/
;; Need to run M-x pdf-tools-install
(require 'pdf-view)

(setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")

(setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                 ,(face-attribute 'default :background)))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(add-hook 'pdf-view-mode-hook (lambda ()
                                (pdf-view-midnight-minor-mode)
				(auto-revert-mode))) ;; Display changes live

(provide 'init-pdfview)
