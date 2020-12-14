(with-eval-after-load 'projectile
	(setq projectile-project-search-path my/project-dir)
	(setq projectile-enable-caching t)
	(setq projectile-globally-ignored-directories '(".stack-work" ".git" "straight" "elpa" ".vs"))
	(setq projectile-globally-ignored-files '("*~" "*.elc" "#*#"))
	(setq projectile-completion-system 'ivy)

	(add-hook 'projectile-mode-hook (lambda ()
																		(local-set-key (kbd "C-o") 'projectile-find-file)
																		(local-set-key (kbd "C-b") 'projectile-switch-to-buffer))))

(straight-use-package 'projectile)
(my/load-my "ivy")

(add-hook 'prog-mode 'projectile-mode)

(my/installed "projectile")
