(with-eval-after-load 'org
	(setq org-todo-keywords
				'((sequence "TODO" "INFO" "WIP" "|" "DONE"))
				org-support-shift-select t))

(straight-use-package 'org)

(my/installed "org")
