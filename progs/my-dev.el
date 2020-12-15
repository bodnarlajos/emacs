(straight-use-package 'flycheck)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'diff-hl)
(straight-use-package 'dap-mode)
;; (straight-use-package 'aggressive-indent)
(straight-use-package 'highlight-indent-guides)
(straight-use-package 'smartparens)
(straight-use-package 'treemacs)
(straight-use-package 'company)
(straight-use-package 'rg)

(my/load-my "ivy")
(my/load-my "jump")

(with-eval-after-load 'highlight-indent-guides
	(custom-set-variables
	 '(highlight-indent-guides-method 'bitmap)))

(with-eval-after-load 'ediff
	(setq ediff-window-setup-function 'ediff-setup-windows-plain-merge)
	(setq ediff-split-window-function 'split-window-horizontally))

(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (custom-set-variables
   '(lsp-ui-sideline-ignore-duplicate t))
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-doc-delay 2.0)
  (setq lsp-ui-doc-hide-delay 2)
  (setq lsp-ui-doc-use-webkit nil)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-header nil)
  (setq lsp-ui-doc-include-signature nil)
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-diagnostic-max-lines 5)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (custom-set-faces
   '(lsp-ui-sideline-global ((t (:inverse-video t :height 0.6)))))
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-border "black")
  (add-hook 'lsp-ui-doc-frame-hook
						(lambda (frame _w)
							(set-face-attribute 'default frame 
																	:font "Noto Sans Mono" 
																	:height 120)))
  (setq lsp-ui-doc-alignment 'frame))

(with-eval-after-load 'lsp-ui-imenu
	(setq lsp-ui-imenu--custom-mode-line-format nil)
	(add-hook 'lsp-ui-imenu-mode-hook #'my/prog-mode-keys))
(with-eval-after-load 'flycheck
	(defun d/flycheck-lighter (state)
		"Return flycheck information for the given error type STATE.
   Source: https://git.io/vQKzv"
		(let* ((counts (flycheck-count-errors flycheck-current-errors))
					 (errorp (flycheck-has-current-errors-p state))
					 (err (or (cdr (assq state counts)) "?"))
					 (running (eq 'running flycheck-last-status-change)))
			(if (or errorp running) (format " FlyC: •%s" err))))
	(setq-default flycheck-highlighting-mode 'lines)
	(define-fringe-bitmap 'flycheck-fringe-bitmap-ball
		(vector #b00000000
						#b00000000
						#b00000000
						#b00000000
						#b00000000
						#b00000000
						#b00000000
						#b00011100
						#b00111110
						#b00111110
						#b00111110
						#b00011100
						#b00000000
						#b00000000
						#b00000000
						#b00000000
						#b00000000))
	(flycheck-define-error-level 'error
		:severity 2
		:overlay-category 'flycheck-error-overlay
		:fringe-bitmap 'flycheck-fringe-bitmap-ball
		:fringe-face 'flycheck-fringe-error)
	(flycheck-define-error-level 'warning
		:severity 1
		:overlay-category 'flycheck-warning-overlay
		:fringe-bitmap 'flycheck-fringe-bitmap-ball
		:fringe-face 'flycheck-fringe-warning)
	(flycheck-define-error-level 'info
		:severity 0
		:overlay-category 'flycheck-info-overlay
		:fringe-bitmap 'flycheck-fringe-bitmap-ball
		:fringe-face 'flycheck-fringe-info)
	(make-local-variable 'truncate-string-ellipsis)
	(setq truncate-string-ellipsis "…")
	(setq flycheck-check-syntax-automatically '(save))

  ;; Check only when saving or opening files. Newline & idle checks are a mote
  ;; excessive and can catch code in an incomplete state, producing false
  ;; positives, so we removed them.
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.9)
	(custom-set-variables
	 '(flycheck-display-errors-function nil)
	 '(flycheck-error-list-minimum-level (quote warning))
	 '(flycheck-highlighting-mode (quote lines))))

(defun my/local-prog-mode ()
	"T."
	(message "local prog mode")
	(setq-local tab-width 2)
	(display-line-numbers-mode)
	(highlight-indent-guides-mode)
	(diff-hl-mode)
	(require 'company-capf)
	(flycheck-mode)
	(smartparens-mode)
	;;(aggressive-indent-mode)
	(company-mode))

(defun flycheck-list-by (what)
	"T."
	(interactive "sWhat (error: e, warning: w): ")
	(when (string-equal what "w")
		(custom-set-variables
		 '(flycheck-error-list-minimum-level (quote warning))))
	(when (string-equal what "e")
		(custom-set-variables
		 '(flycheck-error-list-minimum-level (quote error))))
	(kill-buffer-if-run "*Flycheck errors*")
	(flycheck-list-errors))
(custom-set-variables
 '(lsp-prefer-flymake nil))

(defun my/toggle-lsp-ui-doc ()
  (interactive)
  (if lsp-ui-doc--bounds
			(lsp-ui-doc-hide)
    (lsp-ui-doc-show)))

(setq compilation-auto-jump-to-first-error nil)
(setq compilation-ask-about-save nil)
;; Stop on the first error.
(setq compilation-scroll-output 'next-error)
;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)
(add-hook 'prog-mode-hook 'my/local-prog-mode)

(my/installed "dev")
