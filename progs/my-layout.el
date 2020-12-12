(defun count-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown. Defaults to selected frame."
  (length (mapcar #'window-buffer (window-list frame))))

(defun do-not-split-more-than-two-windows (window &optional horizontal)
  (if (and horizontal (> (count-visible-buffers) 1))
      nil
    t))

(advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-windows)

(defvar my/window-no-other-no-delete
	'(window-parameters . ((no-other-window . t)
												 (no-delete-other-windows . t))))
(defvar my/window-no-delete
	'(window-parameters . ((no-delete-other-windows . t))))
(defun my/show-left-top (buffer alist)
  "T."
	(message "bname: %s" (buffer-name buffer))																 
	(let ((leftTopWin (frame-first-window)))
		(window--display-buffer buffer leftTopWin 'reuse)))
(defun my/largest-other-window (buffer alist)
  "T."
  (let ((sorted-windows (sort (window-list)
															(lambda (a b)
																(> (* (window-width a) (window-height a)) (* (window-width b) (window-height b))))))
				(firstLargeBuffer (car sorted-windows)))
		(catch '--cl-block-display-buffer-largest-other-window--
			(unless (or (equal sw (selected-window))
    							(window-minibuffer-p firstLargeBuffer))
				(window--display-buffer buffer firstLargeBuffer 'reuse)))))
(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)
(setq switch-to-buffer-obey-display-actions t)
(setq
 display-buffer-alist
 `(("\\*\\(?:magit-log.+\\|Ibuffer\\)\\*"
		display-buffer-in-side-window
		(side . right) (slot . -1) (window-width . 0.4) ;; (preserve-size . (700 . t))
		,my/window-no-delete)
	 ("\\*\\(?:undo-tree\\)\\*"
		display-buffer-in-side-window
		(side . left) (slot . -1) (window-width . fit-window-to-buffer) (preserve-size . (nil . t))
		,my/window-no-delete)
	 ("\\*\\(?:eshell\\|hydra\\|ansi-term\\|terminal\\|Flycheck error messages\\|helm flycheck\\|Flycheck errors\\|Minibuf-0\\|rg\\|Minibuf-1\\|magit-revision\\|hydra\\)\\*"
		display-buffer-in-side-window
		(side . bottom) (window-height . 0.3) (slot . 1) (preserve-size . (nil . t))
		,my/window-no-delete)
	 ("\\*\\(?:compilation\\|haskell-compilation\\|eldoc\\)\\*"
		display-buffer-in-side-window
		(side . bottom) (window-height . 0.3) (slot . 2) (preserve-size . (nil . t))
		,my/window-no-delete)
	 ("\\*\\(?:help\\|grep\\|Completions\\|Compile-Log\\|IList\\)\\*"
		display-buffer-in-side-window
		(side . bottom) (slot . 1) (preserve-size . (nil . t))
		,my/window-no-other-no-delete)))

(my/installed "layout")
