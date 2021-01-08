(defconst is-windows (string-equal system-type "windows-nt"))
(defvar my/project-dir "~/Projects")
(message "is-windows: %s" is-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(projectile dap-mode diff-hl lsp-ui lsp-mode alert undo-tree smex ido-vertical-mode vlf js2-mode magit tide dumb-jump company rg)))
(add-to-list 'load-path "/home/lbodnar/.emacs.d/progs")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(ido-mode 1)
(icomplete-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(cua-mode 1)

(load-theme 'tsdh-light)

(setq inhibit-startup-message t)
(if is-windows
    (set-frame-font "Fira Code-10" nil t)
  (set-frame-font "Fira Code-12" nil t))

(require 'ido-vertical-mode)
(require 'my-defun)
(require 'my-setq-defaults)
(require 'my-hydra)
(require 'my-jump)
(require 'my-keys)
(require 'my-layout)
(require 'my-long)
(require 'my-magit)
(require 'company)
(require 'my-compile)
(require 'my-dev)
(require 'my-haskell)
(require 'my-js)
(require 'my-projectile)
(require 'my-web)

(ido-vertical-mode)
(global-undo-tree-mode)
(global-set-key (kbd "M-x") 'smex)
(global-company-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
