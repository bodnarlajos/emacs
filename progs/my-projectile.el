(with-eval-after-load 'projectile
  (setq projectile-project-search-path my/project-dir)
	(setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories '(".stack-work" ".git" "straight" "elpa" ".vs"))
	(setq projectile-globally-ignored-files '("*~" "*.elc" "#*#"))
  (setq projectile-completion-system 'ivy)

  (my/load-my "ivy")
  
  (defun smart-for-files ()
    "T."
    (interactive)
    (if (projectile-project-p)
				(counsel-projectile-find-file)
      (if buffer-file-name
					(counsel-find-file (file-name-directory buffer-file-name))
				(counsel-find-file "~/"))))
  (global-set-key (kbd "C-o") 'smart-for-files)
  (defun smart-for-buffers ()
    "T."
    (interactive)
    (if (projectile-project-p)
				(counsel-projectile)
      (call-interactively 'counsel-switch-to-buffer))))

(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)

(add-hook 'prog-mode 'projectile-mode)

(my/installed "projectile")

