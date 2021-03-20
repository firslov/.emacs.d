;; Add Packages
(defvar my/packages '(
		      helm-swoop
		      htmlize
		      atom-one-dark-theme
		      org2ctex
		      org-roam-bibtex
		      org-ref
		      f
		      all-the-icons
		      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (cl-loop for pkg in my/packages
	   when (not (package-installed-p pkg)) return nil
	   finally return t))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; (use-package evil
;;   :ensure t
;;   :config
;;   (evil-mode 1))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package counsel
  :ensure t)
(use-package swiper
  :ensure t
  :bind (
	 ("C-s" . swiper)
	 ("M-n" . next-error)
	 ("M-p" . previous-error)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> o" . counsel-describe-symbol)
	 ("<f1> l" . counsel-find-library)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 )
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-use-virtual-buffers t
	enable-recursive-minibuffers t))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c n p") 'projectile-command-map)
  (setq projectile-indexing-method 'native
	projectile-completion-system 'ivy))

(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
    :defer t
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :functions my-pdf-view-set-midnight-colors
    :commands pdf-view-midnight-minor-mode
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :hook (after-load-theme . my-pdf-view-set-dark-theme)
    :bind (:map pdf-view-mode-map
		("C-s" . isearch-forward))
    :init
    (add-to-list 'org-file-apps '("\\.pdf\\'" . pdf-view))
    (setq pdf-annot-activate-created-annotations t)

    (defun my-pdf-view-set-midnight-colors ()
      "Set pdf-view midnight colors."
      (setq pdf-view-midnight-colors
	    `(,(face-foreground 'default) . ,(face-background 'default))))

    (defun my-pdf-view-set-dark-theme ()
      "Set pdf-view midnight theme as color theme."
      (my-pdf-view-set-midnight-colors)
      (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (when (eq major-mode 'pdf-view-mode)
	    (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))
    :config
    ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (pdf-tools-install t nil t t)

    (my-pdf-view-set-midnight-colors)

    ;; FIXME: Support retina
    ;; @see https://emacs-china.org/t/pdf-tools-mac-retina-display/10243/
    ;; and https://github.com/politza/pdf-tools/pull/501/
    (setq pdf-view-use-scaling t
	  pdf-view-use-imagemagick nil)
    (with-no-warnings
      (defun pdf-view-use-scaling-p ()
	"Return t if scaling should be used."
	(and (or (and (eq system-type 'darwin) (string-equal emacs-version "27.0.50"))
		 (memq (pdf-view-image-type)
		       '(imagemagick image-io)))
	     pdf-view-use-scaling))
      (defun pdf-view-create-page (page &optional window)
	"Create an image of PAGE for display on WINDOW."
	(let* ((size (pdf-view-desired-image-size page window))
	       (width (if (not (pdf-view-use-scaling-p))
			  (car size)
			(* 2 (car size))))
	       (data (pdf-cache-renderpage
		      page width width))
	       (hotspots (pdf-view-apply-hotspot-functions
			  window page size)))
	  (pdf-view-create-image data
				 :width width
				 :scale (if (pdf-view-use-scaling-p) 0.5 1)
				 :map hotspots
				 :pointer 'arrow))))

    ;; Recover last viewed position
    (use-package pdf-view-restore
      :hook (pdf-view-mode . pdf-view-restore-mode)
      :init (setq pdf-view-restore-filename
		  (locate-user-emacs-file ".pdf-view-restore")))))

;; (add-to-list 'load-path "~/.emacs.d/git-repo/nano-emacs/")
;; (require 'nano)

(use-package org-html-themify
  :load-path "~/.emacs.d/git-repo/org-html-themify"
  )

(setq org-html-themify-themes
      '((dark . atom-one-dark)
	(light . doom-flatwhite)))

(add-hook 'org-mode-hook 'org-html-themify-mode)

(require 'phd)
(use-package org-elp
  :ensure t
  :config
  (setq org-elp-split-fraction 0.2
	org-elp-buffer-name "*Equation Live*"
	org-elp-idle-time 0.5))
(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))
(require 'org2ctex)
;;(org2ctex-toggle t)
(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-global-PDF-mode t TeX-engine 'xetex)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-command-default "XeLaTeX")
  ;; revert pdf-view after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package dashboard
  :ensure t
  :if (< (length command-line-args) 2)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
			  ;; (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5)
			  ;; (registers . 5)
			  ))
  (dashboard-modify-heading-icons '((recents . "file-text") 
				    (bookmarks . "book")))
  ;; 设置标题
  (setq dashboard-banner-logo-title
	"人生苦短，我用Emacs")
  ;; 设置banner
  (setq dashboard-startup-banner "~/.emacs.d/var/banner.png")
  (setq dashboard-center-content t) 
  (setq dashboard-set-heading-icons t) 
  (setq dashboard-set-navigator t)
  ;; (add-hook 'after-init-hook (lambda () (dashboard-refresh-buffer)))
  )

(use-package ace-window
  :ensure t
  :config
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inderit ace-jump-face-foreground :height 3.0))))))

(use-package restart-emacs
  :ensure t
  :defer t
  :bind ("<f12>" . restart-emacs)
  ;; :init
  ;; (defun b-restart-emacs (f)
  ;;   (org-babel-tangle-file "~/.emacs.d/readme.org"))
  ;; (advice-add #'restart-emacs :before #'b-restart-emacs)
  )

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package highlight-parentheses
  :ensure t
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))

(use-package diminish
  :ensure t
  :diminish (ivy-mode eldoc-mode which-key-mode))

(require 'auto-save)
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我

(require 'auto-indent)
(auto-indent-disable)

(require 'auto-load)

(add-to-list 'load-path "~/.emacs.d/git-repo/awesome-tray/")
(require 'awesome-tray)
(setq awesome-tray-info-padding-right 2
      awesome-tray-active-modules '("last-command" "location" "parent-dir" "mode-name" "battery" "date"))
(use-package htmlize
  :custom
  (htmlize-face-overrides '(clojure-keyword-face (:foreground "var(--clr-constant)" :background "var(--bg-constant)"))))

(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :defer 1
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-item 10))

(provide 'init-package)
