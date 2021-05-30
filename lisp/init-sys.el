(when (eq system-type 'windows-nt)
  (setq org-directory "e:/org/"))

(when (eq system-type 'darwin)
  (setq org-directory "~/firslov"))

(when (eq system-type 'gnu/linux)
  (setq org-directory "~/org/"))

(provide 'init-sys)
