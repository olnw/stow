;;; latexmk-mode.el --- LatexMK minor mode
;;; Commentary:
;;; none
;;; Code:

(define-minor-mode latexmk-mode
  "Toggle LatexMK mode."
  :init-value nil
  :lighter " LatexMK "
)

;; --shell-escape necessary for GNU plot
(defun my/run-latexmk ()
  (interactive)
  (start-process "latexmk" "latexmk out" "latexmk" "--shell-escape" "--silent" "--pdf" (buffer-file-name (current-buffer)))
)

(defun my/try-run-latexmk ()
    "Try to run latexmk."
  
  (if (bound-and-true-p latexmk-mode)
      (my/run-latexmk)
    )
  )
  
(add-hook 'after-save-hook 'my/try-run-latexmk)
(add-hook 'latex-mode-hook 'latexmk-mode)
;;; latexmk-mode.el ends here
