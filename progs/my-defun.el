(defun my/install (pkg)
  "install a package if dont'"
  (interactive)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun my/open-notes ()
	"Open file from the notes directory"
	(interactive)
	(if (find-file (concat "~/Documents/notes/" (ido-completing-read "Find a note: " (directory-files "~/Documents/notes"))))
      (message "Opening note...")
    (message "Aborting")))

(defun my/start-later (delay func)
  "T."
  (run-with-idle-timer delay nil func))

(defun my/load-my (filename)
  "T."
  (when (not (member filename my/pkg-loaded))
    (message "load module: %s" filename)
    (let ((my-load-file
					 (expand-file-name (concat "progs/my-" filename ".el") user-emacs-directory)))
      (load my-load-file))))

(defun my/installed (module)
  "The modul was installed"
	(when (not (member module my/pkg-loaded))
	  (message "add module %s" module)
		(add-to-list 'my/pkg-loaded module)))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun duplicate-line()
	"T."
	(interactive)
	(move-beginning-of-line 1)
	(kill-line)
	(yank)
	(open-line 1)
	(next-line 1)
	(yank))

(defun parent-directory (dir)
	(unless (equal "/" dir)
		(file-name-directory (directory-file-name dir))))

(defun log-highlight ()
	"T."
	(interactive)
	(highlight-lines-matching-regexp "\\(\\[Debug.*\\]\\)" 'hi-blue-b)
	(highlight-lines-matching-regexp "\\(\\[Error.*\\]\\)" 'hi-red-b)
	(highlight-lines-matching-regexp "\\(Exception.*\\)" 'hi-red-b)
	(highlight-lines-matching-regexp "\\(\\[Info.*\\]\\)" 'hi-green-b))

(defun open-note ()
	"T."
	(interactive)
	(find-file "~/note.org"))

(defun switch-to-previous-buffer ()
	"Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
	(interactive)
	(switch-to-buffer (other-buffer (current-buffer) 1)))

(defun crux-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (crux-smart-open-line-above)
    (move-end-of-line nil)
    (newline-and-indent)))

(defun crux-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (crux-move-to-mode-line-start) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (forward-line -1)
    (indent-according-to-mode)))

(defun kill-buffer-if-run (bufferName)
	"T."
	(let ((bl (buffer-list)))
		(while bl
			(when (string-equal (buffer-name (car bl)) bufferName)
				(kill-buffer (car bl))
				(setq bl nil))
			(setq bl (cdr bl)))))

(defun crux-smart-kill-line ()
  "Kill to the end of the line and kill whole line on the next call."
  (interactive)
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (if (= orig-point (point))
        (kill-whole-line)
      (goto-char orig-point)
      (kill-line))))

(defvar my/temp-file-path "/tmp/")
(defvar my/temp-buffer-prefix "#:")
(defun my/find-file-with-temp (filepath)
	"T."
	(interactive "FOpen file: ")
	(let ((temp-path (concat my/temp-file-path (string-trim-left filepath "^\/"))))
		(message "%s" temp-path)
		(async-shell-command (concat "mkdir -p " (file-name-directory temp-path) "; cat " filepath " > " temp-path))
		(find-file (concat temp-path))))

(defun my/view-file (filepath)
	"Open file async, it's just readonly and you can't revert"
	(interactive "FOpen file: ")
	(let ((temp-buffer-name (generate-new-buffer-name (concat my/temp-buffer-prefix filepath) (concat my/temp-buffer-prefix filepath))))
		(switch-to-buffer temp-buffer-name)
		(async-shell-command (concat "cat " filepath) temp-buffer-name)))

(defun my/revert-view-file ()
	"Revert a view-file"
	(interactive)
	(let* ((buffertitle (buffer-name))
				 (bufferpath (string-remove-prefix my/temp-buffer-prefix buffertitle)))
		(if (string-prefix-p my/temp-buffer-prefix buffertitle)
				(progn
					(message "%s" bufferpath)
					(my/view-file bufferpath))
			(message "It's not a view-file: %s" buffertitle))))

(defun my/view-file-at-line ()
	"Open a file async based on the current line"
	(interactive)
	(let ((filepath (string-trim (buffer-substring (line-beginning-position) (line-end-position)))))
		(if (file-exists-p filepath)
				(my/view-file filepath)
			(message "It's not a file: %s" filepath))))

(my/installed "defun")
