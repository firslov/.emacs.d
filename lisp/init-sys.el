(when (eq system-type 'windows-nt)
  (setq org-directory "e:/org/"
	org-roam-directory org-directory))

(when (eq system-type 'darwin)
  (setq org-directory "~/firslov"
	org-roam-directory org-directory))

(when (eq system-type 'gnu/linux)
  (setq org-directory "~/org/"
	org-roam-directory org-directory))

(provide 'init-sys)
