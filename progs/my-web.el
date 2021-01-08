(my/install 'web-mode)
(my/install 'less-css-mode)

;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "jsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

(provide 'my-web)
