;; cl - Common Lisp Extension
(require 'cl)
(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (doom-one)))
 '(custom-safe-themes
   (quote
    ("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" default)))
 '(doom-modeline-minor-modes t)
 '(neo-mode-line-type (quote default))
 '(package-selected-packages
   (quote
    (which-key use-package all-the-icons neotree org-download highlight-parentheses youdao-dictionary doom-themes doom-modeline ivy swiper exec-path-from-shell)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inderit ace-jump-face-foreground :height 3.0)))))
