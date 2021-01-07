(defconst is-windows (string-equal system-type "windows-nt"))
(message "is-windows: %s" is-windows)
(custom-set-variables
 '(package-selected-packages
	 (quote
	  (undo-tree smex ido-vertical-mode vlf js2-mode magit tide dumb-jump company rg))))
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
(ido-vertical-mode)
(global-undo-tree-mode)
(global-set-key (kbd "M-x") 'smex)

(require 'my-defun)
