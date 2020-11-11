;; cl - Common Lisp Extension
(require 'cl-lib)
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/")))
  (package-initialize))
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Add use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package org
  :ensure t)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package auto-load)
(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :defer 1
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-item 10))
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package org-download
  :ensure t
  :defer t
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "./src")
  (setq org-download-display-inline-images nil))

(use-package highlight-parentheses
  :ensure t
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))

(use-package youdao-dictionary
  :ensure t
  :defer t
  :config
  ;; enable cache
  (setq url-automatic-caching t))

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
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; (use-package doom-themes
;;   :ensure t)
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (emacs-startup . doom-modeline-mode)
;;   :config
;;   (setq
;;    ;; inhibit-compacting-font-caches t
;;    ;; doom-modeline-height 1
;;    doom-modeline-buffer-file-name-style 'auto
;;    ;; doom-modeline-icon nil
;;    ;; doom-modeline-project-detection 'project
;;    ))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-indexing-method 'native)
  (setq projectile-completion-system 'ivy))

(use-package phd
  :defer t)
(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
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
(use-package interleave
  :defer t
  :ensure t
  :config
  (setq interleave-split-direction 'horizontal)
  (setq interleave-split-lines 9))

(use-package lazycat-theme
  :load-path "~/.emacs.d/lisp/lazycat-theme-master")

(use-package awesome-tray
  :load-path "~/.emacs.d/lisp/awesome-tray"
  :init (setq awesome-tray-active-modules '("parent-dir" "mode-name" "git" "date"))
  :config
  (awesome-tray-mode 1)
  (lazycat-theme-load-dark))

(use-package awesome-tab
  :load-path "~/.emacs.d/lisp/awesome-tab"
  :config
  (awesome-tab-mode t))

(use-package all-the-icons
  :ensure t) 
(use-package restart-emacs
  :ensure t
  :defer t
  :init
  (defun b-restart-emacs (f)
    (org-babel-tangle-file "~/.emacs.d/readme.org" "~/.emacs.d/init.el"))
  (advice-add #'restart-emacs :before #'b-restart-emacs))
(use-package ace-window
  :ensure t
  :defer t
  :config
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inderit ace-jump-face-foreground :height 3.0))))))
(use-package diminish
  :ensure t
  :diminish (ivy-mode eldoc-mode which-key-mode))
(use-package org-equation-live-preview
  :defer t)
(use-package learn-timer
  :load-path "~/.emacs.d/lisp/learn-timer/"
  :after awesome-tray
  :config
  (add-to-list 'awesome-tray-active-modules "timer" 'append)
  (add-to-list 'awesome-tray-active-modules "todo" 'append)
  )
(use-package auto-save
  :config
  (auto-save-enable)              ;; 开启自动保存功能
  (setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我
  )
(use-package auto-indent
  :config (auto-indent-enable))
;; (use-package init-site)

(use-package elpy
  :ensure t
  :defer t
  :config
  (elpy-enable)
  ;; Use IPython for REPL
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
	       "jupyter"))

(use-package py-autopep8
  :ensure t
  :defer t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  (setq py-autopep8-options '("--max-line-length=100")))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (global-flycheck-mode)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package ein
  :ensure t
  ;;:defer t
  )

(when (eq system-type 'windows-nt)
  (setq conf_dir "e:/emacs/.emacs.d/"
	org-directory "e:/org/"
	init-file (concat conf_dir "lisp/init-main.el")
	init-sys (concat conf_dir "lisp/windows-nt.el"))
  ;; init fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )

(when (eq system-type 'darwin)
  (setq conf_dir "~/.emacs.d/"
	org-directory "~/org/"
	init-file (concat conf_dir "lisp/init-main.el")
	init-sys (concat conf_dir "lisp/darwin.el"))
  ;; font
  (set-face-attribute 'default nil :font "MesloLGLDZ Nerd Font 15")
  ;; env
  (setenv "PATHONPATH" "/opt/anaconda3/bin")
  (setenv "PATH" "/opt/anaconda3/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin")
  (setq python-shell-interpreter "/opt/anaconda3/bin/python3")
  )

(when (eq system-type 'gnu/linux)
  (setq conf_dir "~/.emacs.d/"
	org-directory "~/org/"
	init-file (concat conf_dir "lisp/init-main.el")
	init-sys (concat conf_dir "lisp/darwin.el"))
  )

;; 启动页面
;; (setq initial-buffer-choice (concat org-directory "note.org"))
;; 强制左右分屏
(setq split-height-threshold nil)
(setq split-width-threshold 0)
;; 关闭欢迎界面
(setq inhibit-splash-screen t)
;; fonts problem
(setq inhibit-compacting-font-caches t)
;; Enable line numbers globally
;; (global-linum-mode nil)
;; disable alert voice
(setq ring-bell-function 'ignore)
;; UI
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode t)
;; Highlight the "()"
(show-paren-mode t)
;; Auto complete the "()"
(electric-pair-mode t)
;; Set the electric-pair-mode's pair keywords
(setq electric-pair-pairs
      '((?\" . ?\")
	(?\( . ?\))
	(?\< . ?\>)
	(?\{ . ?\})))
(setq-default cursor-type 'bar)
;; Save the point position
(save-place-mode t)
;; 设置默认读入文件编码
(prefer-coding-system 'gbk)
;; 设置写入文件编码
(setq default-buffer-file-coding-system 'gbk)
;; disable backup
(setq make-backup-files nil)
;; disable auto-save
(setq auto-save-default nil)
;; yes-or-no y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
;; select input delete
(delete-selection-mode 1)
;; highlight current line
(global-hl-line-mode 1)
;; autoload change out of emacs
(global-auto-revert-mode 1)
;; highlight in org-mode
(setq org-src-fontify-natively t)
;; 自动换行
(toggle-truncate-lines 1)
;; emacs guess indent
(setq python-indent-guess-indent-offset nil)
;; display buffer alist
(setq display-buffer-alist
      '(("\\*e?shell\\*"
	 (display-buffer-in-side-window)
	 (window-height . 0.3)
	 (side . bottom)
	 (slot . -1))
	("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\)\\*"
	 (display-buffer-in-side-window)
	 (window-width . 0.5)
	 (side . right)
	 (slot . 1))
	))

;; save all buffers
(global-set-key (kbd "<f12>") 'org-save-all-org-buffers)
;; 绑定 <f5> 键上
(global-set-key (kbd "<f5>") 'youdao-dictionary-search-at-point)
;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; show startup page
(global-set-key (kbd "C-.") 'show-startup-page)
;; 将函数 load-init-file 绑定到 <f1> 0 键上
(global-set-key (kbd "<f1> 0") 'load-init)
;; 将函数 open-init-file 绑定到 <f1> 1 键上
(global-set-key (kbd "<f1> 1") 'open-init-file)
;; 将函数 open-articles 绑定到<f1> 2 键上
(global-set-key (kbd "<f1> 2") 'open-articles)
;; 将函数 org-todo 绑定到<f1> 3 键上
(global-set-key (kbd "<f1> 3") 'org-todo-list)
;; 将函数 indent-buffer 绑定到 <f8> 键上
(global-set-key (kbd "<f8>") 'indent-buffer)
;; 上下翻半页
(global-set-key "\M-n" 'scroll-half-page-up)
(global-set-key "\M-p" 'scroll-half-page-down)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file (concat conf_dir "readme.org")))
;; 快速加载配置文件
(defun load-init()
  (interactive)
  (org-babel-load-file (concat conf_dir "readme.org")))
;; 快速打开articles
(defun open-articles()
  (interactive)
  (find-file "~/Documents/org/Archive.org"))
;; neotree list config-dir
(defun nconf()
  (interactive)
  (neotree-dir conf_dir)
  (other-window -1))
;; format the buffer
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))
;; 翻页
(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))
(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))
;; 窗口启动位置大小
(defun init-my-frame ()
  (set-frame-position (selected-frame) 160 80)
  (set-frame-width (selected-frame) 120)
  (set-frame-height (selected-frame) 30))
(add-hook 'after-init-hook 'init-my-frame)
;; set alpha
(defun set-alpha (var)
  "Set the backgroud alpha by VAR."
  (interactive "sAlpha or not(y-or-n): ")
  (pcase var
    ("y" (set-frame-parameter nil 'alpha '(90 . 100)))
    ("n" (set-frame-parameter nil 'alpha '(100 . 100)))))
;; 快速打开blog文件
(defun blog()
  (interactive)
  (find-file "~/site/org/index.org")
  (blog-mode)
  (neotree-dir "~/site/"))
;; refresh startup function
(defun show-startup-page()
  (interactive)
  (org-agenda-list)
  (org-agenda-day-view)
  (neotree-dir "~/Documents/org/")
  (other-window -1))
;; (add-hook 'window-setup-hook 'show-startup-page)

;; blog mode
(define-minor-mode blog-mode
  "Toggle blog mode."
  :global nil
  :lighter "Blog")

;; @purcell
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
	 (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
	 (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(global-set-key (kbd "M-C-8") (lambda ()
				(interactive)
				(sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda ()
				(interactive)
				(sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda ()
				(interactive)
				(modify-frame-parameters nil `((alpha . 100)))))

;; org variables
;; (add-to-list 'org-file-apps '("\\.pdf\\'" . "Microsoft\ edge %s"))
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook 'linum-mode)
(setq org-agenda-files (list (concat org-directory "inbox.org"))
      lt-todo-files org-agenda-files
      org-agenda-skip-function-global '(org-agenda-skip-entry-if 'regexp "\\* DONE\\|\\* CANCELED")
      ;; org-agenda-skip-function-global '(org-agenda-skip-entry-if 'notregexp "\\* DONE\\|\\* CANCELED")
      org-agenda-show-future-repeats nil
      org-deadline-warning-days 30
      org-agenda-window-setup nil
      org-M-RET-may-split-line '((headline . nil))
      org-agenda-time-grid '((daily today require-timed)
			     (1000 1400 1600 2000 2200)
			     "......" "----------------")
      org-capture-templates
      `(("i" "Inbox" entry (file+headline ,(concat org-directory "inbox.org") "Inbox:")
	 "* %?")
	("j" "Journal" entry (file+datetree ,(concat org-directory "journal.org"))
	 "* %U\n%?")
	("a" "Arrangement" entry (file+headline ,(concat org-directory "inbox.org") "Arrangement:")
	 "* %? %^T")
	("t" "Todo")
	("tt" "Todo without time" entry (file+headline ,(concat org-directory "inbox.org") "Todo:")
	 "* TODO %?")
	("ts" "Todo with SCHEDULED" entry (file+headline ,(concat org-directory "inbox.org") "Todo:")
	 "* TODO %?\nSCHEDULED:%^t")
	("td" "Todo with DEADLINE" entry (file+headline ,(concat org-directory "inbox.org") "Todo:")
	 "* TODO %?\nDEADLINE:%^t"))
      ;; org-refile-targets
      ;; `((,(concat org-directory "note.org") :maxlevel . 2))
      ;; `((,(concat org-directory "read.org") :maxlevel . 1)
      ;; (,(concat org-directory "learn.org") :maxlevel . 1)
      ;; (,(concat org-directory "emacs.org") :level . 1))
      org-todo-keywords
      '((sequence "TODO(t)" "CANCELED(c)" "|" "DONE(d)"))
      org-todo-keyword-faces
      '(("DAILY" . "green")
	("CANCELED" . "grey")))
;; set key
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\M-q" 'org-agenda)

(when (eq system-type 'gnu/linux)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(package-selected-packages
     '(ein flycheck py-autopep8 elpy diminish ace-window org-bullets restart-emacs all-the-icons interleave pdf-tools projectile undo-tree yasnippet company counsel youdao-dictionary highlight-parentheses org-download neotree which-key magit exec-path-from-shell use-package))
   '(show-paren-mode t)
   '(tool-bar-mode nil))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Purisa" :foundry "PfEd" :slant normal :weight bold :height 120 :width normal))))
   '(aw-leading-char-face ((t (:inderit ace-jump-face-foreground :height 3.0))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(youdao-dictionary which-key use-package undo-tree restart-emacs py-autopep8 projectile pdf-view-restore org-download org-bullets org neotree magit interleave highlight-parentheses flycheck esup elpy ein diminish counsel company-tabnine benchmark-init all-the-icons ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
