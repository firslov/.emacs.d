(defgroup auto-indent nil
  "Auto indent buffer when emacs idle."
  :group 'auto-indent)

(defcustom auto-indent-idle 1
  "The idle seconds to auto indent buffer."
  :type 'integer
  :group 'auto-indent)

(defcustom auto-indent-mode-list '(org-mode lisp-interaction-mode python-mode emacs-lisp-mode)
  "This the mode list of which uses auto-indent."
  :type 'list
  :group 'auto-indent)

(defun auto-indent-buffer ()
  (interactive)
  (when (and (buffer-modified-p) (member major-mode auto-indent-mode-list))
    (with-temp-message "" (indent-region (point-min) (point-max))))
  )

(defun auto-indent-enable ()
  (interactive)
  (run-with-idle-timer auto-indent-idle t #'auto-indent-buffer)
  )

(provide 'auto-indent)
