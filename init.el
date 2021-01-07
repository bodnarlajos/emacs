(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dap-mode diff-hl lsp-ui lsp-mode alert undo-tree smex ido-vertical-mode vlf js2-mode magit tide dumb-jump company rg)))
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
(set-frame-font "Fira Code-12" nil t)
(require 'ido-vertical-mode)
(ido-vertical-mode)
(global-undo-tree-mode)
(global-set-key (kbd "M-x") 'smex)

(defun ide ()
  "Start an ide funcionality"
  (interactive)
  (require 'my-compile)
  (require 'my-dev)
  (require 'my-haskell))

(global-company-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
