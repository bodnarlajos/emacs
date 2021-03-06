;; (message "%s" file-name-handler-alist)
;; (setq debug-on-error t)
(setq frame-title-format '("%b"))
(setq file-name-handler-alist nil)
(setq inhibit-startup-screen t)

(defvar my/pkg-loaded '("base"))
(defvar bootstrap-version nil)

(defconst is-lbodnar (string-equal system-name "lbodnar"))
(defvar my/project-dir '("/home/lbodnar/Projects"))
(when (not is-lbodnar)
  (setq my/project-dir '("c:/git/mas" "c:/git/mas/WebApiServices")))

(cua-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(if is-lbodnar
    (set-frame-font "Noto Sans Mono-12" t)
  (set-frame-font "Noto Sans Mono-9" t))

(load-theme 'tsdh-light)
(setq completion-ignore-case t
			read-file-name-completion-ignore-case t
			read-buffer-completion-ignore-case t)
;; (setq ido-case-fold nil)
(ido-mode 1)
(make-local-variable 'ido-decorations)
(setf (nth 2 ido-decorations) "\n")
;; show any name that has the chars you typed
(setq ido-enable-flex-matching t)
;; use current pane for newly opened file
(setq ido-default-file-method 'selected-window)
;; use current pane for newly switched buffer
(setq ido-default-buffer-method 'selected-window)
;; stop ido from suggesting when naming new file
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
(setq max-mini-window-height 0.5)

;; ################################################
;; just for defuns and entry-points
;;
;; ################################################

(defun my/packages ()
  "Load straight package manager"
  (interactive)
	(when (not bootstrap-version)
		(message "package init")
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
			(load bootstrap-file nil 'nomessage))))

(defun my/install-modules ()
  "install the base modules"
  (interactive)
	(message "module install")
  (straight-use-package 'undo-tree)
	;;  (straight-use-package 'ace-window)
  (straight-use-package 'rg)
	;;  (straight-use-package 'ivy)
	;;  (straight-use-package 'counsel)
  (straight-use-package 'hydra)
  (straight-use-package 'smex)
  (straight-use-package 'projectile)
  (straight-use-package 'markdown-mode)
  (straight-use-package 'org)
	(message "module install end"))

(global-set-key (kbd "C-M-S-i") 'my/install-modules)

(defun my/default-modules ()
  "Load default plugins"
	(message "default modules loading")
  (let ((my-load-file
				 (expand-file-name "progs/my-defun.el" user-emacs-directory)))
    (load my-load-file))
  (straight-use-package 'undo-tree)
  (global-undo-tree-mode)
  (straight-use-package 'rg)
  (recentf-mode)
	(straight-use-package 'smex)
	;;(straight-use-package 'ace-window)
  (my/load-my "keys")
  (my/load-my "setq-defaults")
	(my/load-my "layout")
	(message "default modules loaded"))

(defun my/entrypoint ()
	"Initialization for entry points"
	(my/packages)
	(my/default-modules))

;; entry point for magit, from commands
(defun my/git ()
  "T."
  (interactive)
  (my/entrypoint)
  (my/load-my "magit")
  (my/magit-status))


;; ******************************************
;; entry points
;; ******************************************

;; entry point for long-line's, from commands
(defun my/long-line ()
	"Open long lines plugins"
	(interactive)
	(let ((ep (line-end-position)))
		(message "check long-line, first crln is: %s" ep)
		(when (>= ep 1000)
			(so-long)
			(let ((my-load-file
						 (expand-file-name "progs/longlines.el" user-emacs-directory)))
				(load my-load-file))
			(longlines-mode))))

(add-hook 'nxml-mode-hook 'my/long-line)
(add-hook 'json-mode-hook 'my/long-line)

;; entry point
(global-set-key (kbd "C-l") 'my/menu)
(defun my/menu ()
  "T."
  (interactive)
  (my/entrypoint)
  (my/load-my "hydra")
  (start-hydra))

;; fallback to M-x
(global-set-key (kbd "C-c m") 'execute-extended-command)
;; entry point with M-x
(global-set-key (kbd "M-x") 'my/M-x)
;; entry point with find-file
(global-set-key (kbd "C-o") 'find-file)
;; entry point with buffers
(global-set-key (kbd "C-b") 'switch-to-buffer)

(defun my/M-x ()
  "Load commands"
  (interactive)
	(global-set-key (kbd "M-x") 'smex)
	(my/packages)
	(my/default-modules)
	(smex-initialize)
	(smex))
;; entry point, automatic load the base packages
;; load packages if we have a little time ...
(run-with-idle-timer 1 nil (lambda ()
														 (my/packages)
														 (if (file-exists-p "~/.emacs.d/straight/build-cache.el")
																 (my/default-modules)
															 (my/install-modules))))

;; entry point, js file
(defun init-js ()
	"T."
	(my/entrypoint)
	(setq auto-mode-alist (delete '("\\.js\\'" . init-js) auto-mode-alist))
	(my/load-my "js")
	(js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . init-js))

;; entry point, ts file
(defun init-ts ()
	"T."
	(my/entrypoint)
	(setq auto-mode-alist (delete '("\\.ts\\'" . init-ts) auto-mode-alist))
	(my/load-my "js")
	(typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . init-ts))

;; entry point, cs file
(defun init-cs ()
	"T."
	(my/entrypoint)
	(setq auto-mode-alist (delete '("\\.cs\\'" . init-cs) auto-mode-alist))
	(my/load-my "csharp")
	(csharp-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . init-cs))

;; entry point, html file
(defun init-html ()
	"T."
	(my/entrypoint)
	(setq auto-mode-alist (delete '("\\.(html\\|htm\\|cshtml\\|jsx)\\'" . init-html) auto-mode-alist))
	(my/load-my "web")
	(web-mode))
(add-to-list 'auto-mode-alist '("\\.(html\\|htm\\|cshtml\\|jsx)\\'" . init-html))

;; entry point, less, css file
(defun init-less ()
	"T."
	(my/entrypoint)
	(setq auto-mode-alist (delete '("\\.\\(less\\|css\\)\\'" . init-less) auto-mode-alist))
	(my/load-my "web")
	(less-css-mode))
(add-to-list 'auto-mode-alist '("\\.\\(less\\|css\\)\\'" . init-less))

;; entry point, haskell
(defun init-hs ()
	"T."
	(my/entrypoint)
	(setq auto-mode-alist (delete '("\\.\\(hs\\)\\'" . init-hs) auto-mode-alist))
	(my/load-my "haskell")
	(haskell-mode))
(add-to-list 'auto-mode-alist '("\\.\\(hs\\)\\'" . init-hs))

;; entry point, shakespeare
(defun init-shakespeare ()
	"T."
	(my/entrypoint)
	(my/load-my "haskell"))

(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . (lambda ()
																									 (init-shakespeare)
																									 (shakespeare-hamlet-mode))))
(add-to-list 'auto-mode-alist '("\\.julius\\'" . (lambda ()
																									 (init-shakespeare)
																									 (shakespeare-julius-mode))))
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . (lambda ()
																									 (init-shakespeare)
																									 (shakespeare-lucius-mode))))
;; entry point, json file
(defun init-json ()
	"T."
	(my/entrypoint)
	(setq auto-mode-alist (delete '("\\.el\\'" . init-md) auto-mode-alist))
	(straight-use-package 'json-mode)
	(json-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . init-json))

;; entry point, markdown file
(defun init-md ()
	"T."
	(my/entrypoint)
	(straight-use-package 'markdown-mode)
	(setq auto-mode-alist (delete '("\\.md\\'" . init-md) auto-mode-alist))
	(markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . init-md))

;; entry point, emacslisp file
(defun init-elisp ()
	"T."
	(my/entrypoint)
	(setq auto-mode-alist (delete '("\\.el\\'" . init-elisp) auto-mode-alist))
	(my/load-my "dev")
	(emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . init-elisp))
