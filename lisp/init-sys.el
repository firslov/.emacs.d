(when (eq system-type 'windows-nt)
  (setq conf_dir "e:/emacs/.emacs.d/"
	org-directory "e:/org/"))

(when (eq system-type 'darwin)
  (setq conf_dir "~/.emacs.d/"
	org-directory "~/firslov"))

(when (eq system-type 'gnu/linux)
  (setq conf_dir "~/.emacs.d/"
	org-directory "~/org/"))

(provide 'init-sys)
