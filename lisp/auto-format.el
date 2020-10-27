(defgroup auto-format nil
  "Auto format buffer when emacs idle."
  :group 'auto-format)

(defcustom auto-format-idle 1
  "The idle seconds to auto format buffer."
  :type 'integer
  :group 'auto-format)

(defcustom auto-format-mode-list '(org-mode lisp-interaction-mode python-mode emacs-lisp-mode)
  "This the mode list of which uses auto-format."
  :type 'list
  :group 'auto-format)

(defun auto-format-buffer ()
  (interactive)
  (when (and (buffer-modified-p) (member major-mode auto-format-mode-list))
    (with-temp-message "" (indent-region (point-min) (point-max))))
  )

(defun auto-format-enable ()
  (interactive)
  (run-with-idle-timer auto-format-idle t #'auto-format-buffer)
  )

(provide 'auto-format)
