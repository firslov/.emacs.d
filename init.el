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

(use-package org-mind
  :load-path "~/.emacs.d/lisp/")

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

(use-package lazycat-theme
  :load-path "~/.emacs.d/git-repo/lazycat-theme")

(use-package awesome-tray
  :load-path "~/.emacs.d/git-repo/awesome-tray"
  :init (setq awesome-tray-active-modules '("parent-dir" "mode-name" "git" "date"))
  :config
  (awesome-tray-mode 1)
  (lazycat-theme-load-dark)
  (setq-default mode-line-format (remove 'mode-line-buffer-identification mode-line-format)))

(use-package phd
  :defer t)
(use-package org-elp
  :load-path "~/.emacs.d/git-repo/org-elp"
  :defer t)
(use-package shengci
  :ensure f
  :load-path "~/.emacs.d/git-repo/shengci.el")

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
	projectile-switch-project-action 'neotree-projectile-action))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package youdao-dictionary
  :ensure t
  :defer t
  :config
  ;; enable cache
  (setq url-automatic-caching t))

(use-package posframe
  :ensure t
  :config
  (defun call-a-posframe ()
    (interactive)
    (defvar my-posframe-buffer " *my-posframe-buffer*")
    (with-current-buffer (get-buffer-create my-posframe-buffer)
      (erase-buffer)
      (insert "Hello world"))
    (when (posframe-workable-p)
      (posframe-show my-posframe-buffer
		     :position (point)))))

(use-package ace-window
  :ensure t
  :defer t
  :config
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inderit ace-jump-face-foreground :height 3.0))))))

(use-package restart-emacs
  :ensure t
  :defer t
  :bind ("<f12>" . restart-emacs)
  :init
  (defun b-restart-emacs (f)
    (org-babel-tangle-file "~/.emacs.d/readme.org" "~/.emacs.d/init.el"))
  (advice-add #'restart-emacs :before #'b-restart-emacs))

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

(use-package all-the-icons
  :ensure t)
(use-package diminish
  :ensure t
  :diminish (ivy-mode eldoc-mode which-key-mode))
(use-package learn-timer
  :load-path "~/.emacs.d/lisp"
  :after awesome-tray
  :config
  (add-to-list 'awesome-tray-active-modules "timer" 'append)
  (add-to-list 'awesome-tray-active-modules "todo" 'append)
  )
(use-package auto-save
  :load-path "~/.emacs.d/lisp"
  :config
  (auto-save-enable)              ;; 开启自动保存功能
  (setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我
  )
(use-package auto-indent
  :load-path "~/.emacs.d/lisp"
  :config (auto-indent-enable))
(use-package auto-load
  :load-path "~/.emacs.d/lisp")
(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :defer 1
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-item 10))

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
  )

(when (eq system-type 'windows-nt)
  (setq conf_dir "e:/emacs/.emacs.d/"
	org-directory "e:/org/"))

(when (eq system-type 'darwin)
  (setq conf_dir "~/.emacs.d/"
	org-directory "~/firslov/")
  ;; font
  (set-face-attribute 'default nil :font "MesloLGLDZ Nerd Font 15")
  ;; (set-face-attribute 'default nil :font "Sarasa Mono SC Nerd 16")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
		      charset
		      (font-spec :family "STKaiti" :size 17)))
  ;; env
  (setenv "PATHONPATH" "/opt/anaconda3/bin")
  (setenv "PATH" "/opt/anaconda3/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin")
  (setq python-shell-interpreter "/opt/anaconda3/bin/python3")
  (setq ein:jupyter-server-command "/opt/anaconda3/bin/jupyter"))

(when (eq system-type 'gnu/linux)
  (setq conf_dir "~/.emacs.d/"
	org-directory "~/org/"))

;; 启动页面
;; (setq initial-buffer-choice (concat org-directory "note.org"))
;; init fullscreen
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
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
	;;(?\< . ?\>)
	(?\{ . ?\})))
(setq-default cursor-type 'bar)
;; Save the point position
(save-place-mode t)
;; 设置默认读入文件编码
;; (prefer-coding-system 'gbk)
;; 设置写入文件编码
;; (setq default-buffer-file-coding-system 'gbk)
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
(toggle-truncate-lines nil)
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
	 (window-height . 0.3)
	 (side . bottom)
	 (slot . 1))
	))
(add-to-list 'org-link-frame-setup '(file . find-file))

;; 绑定 <f5> <f6> 键上
(global-set-key (kbd "<f5>") 'youdao-dictionary-search-at-point-posframe)
(global-set-key (kbd "<f6>") 'youdao-dictionary-play-voice-at-point)
;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; show startup page
(global-set-key (kbd "C-.") 'show-startup-page)
;; 将函数 load-init-file 绑定到 <f1> 0 键上
(global-set-key (kbd "<f1> 0") 'load-init)
;; 将函数 open-init-file 绑定到 <f1> 1 键上
(global-set-key (kbd "<f1> 1") 'open-init-file)
;; 将函数 org-mind-conf 绑定到<f1> 2 键上
(global-set-key (kbd "<f1> 2") 'org-mind-conf)
;; 将函数 org-todo 绑定到<f1> 3 键上
;; (global-set-key (kbd "<f1> 3") 'org-todo-list)
;; 将函数 indent-buffer 绑定到 <f8> 键上
(global-set-key (kbd "<f8>") 'indent-buffer)
;; 上下翻半页
(global-set-key "\M-n" 'scroll-half-page-up)
(global-set-key "\M-p" 'scroll-half-page-down)
;; 生词记录
(global-set-key (kbd "<f7>") 'shengci-capture-word-and-save)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file (concat conf_dir "readme.org")))
;; 快速加载配置文件
(defun load-init()
  (interactive)
  (org-babel-load-file (concat conf_dir "readme.org")))
;; 快速打开articles
(defun org-mind-conf()
  (interactive)
  (find-file "~/.emacs.d/lisp/org-mind.el"))
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
  (set-frame-position (selected-frame) 120 40)
  (set-frame-width (selected-frame) 128)
  (set-frame-height (selected-frame) 32))
(add-hook 'after-init-hook 'init-my-frame)
;; set alpha
(defun set-alpha (var)
  "Set the backgroud alpha by VAR."
  (interactive "sAlpha or not(y-or-n): ")
  (pcase var
    ("y" (set-frame-parameter nil 'alpha '(90 . 100)))
    ("n" (set-frame-parameter nil 'alpha '(100 . 100)))))
;; refresh startup function
(defun show-startup-page()
  (interactive)
  (if (equal (buffer-name) "*Org Agenda*")
      (bury-buffer)
    (progn
      (org-agenda-list)
      (org-agenda-day-view))))

(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\`") 'my/show-todo)))
(define-key org-ql-view-map (kbd "q") 'kill-buffer-and-window)

;;(add-hook 'window-setup-hook 'show-startup-page)
(advice-add 'my/show-todo :after (lambda (&rest r)
				   (shrink-window-horizontally 12)))
;; (advice-add 'bury-buffer :after (lambda (&rest r)
;; 				  (delete-other-windows)
;; 				  (cl-loop while (gnus-buffer-exists-p "*Org Agenda*")
;; 					   do (kill-buffer "*Org Agenda*"))
;; 				  (setq startup-page t)))

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
(setq org-agenda-files (directory-files-recursively "~/firslov/" "\\.org$")
      lt-todo-files (directory-files-recursively "~/firslov/" "\\.org$")
      org-agenda-skip-function-global '(org-agenda-skip-entry-if 'regexp "\\* DONE\\|\\* CANCELED")
      org-deadline-warning-days 30
      org-agenda-window-setup nil
      org-M-RET-may-split-line '((headline . nil))
      org-agenda-time-grid (quote
			    ((daily today require-timed remove-match)
			     (800 1800)
			     "......" "----------------"))
      org-capture-templates
      `(("i" "Inbox" entry (file+headline ,(concat org-directory "inbox.org") "Inbox:")
	 "* %?" :unnarrowed t)
	("j" "Journal" entry (file+datetree ,(concat org-directory "journal.org"))
	 "* %U\n%?" :unnarrowed t)
	;; ("a" "Arrangement" entry (file+headline ,(concat org-directory "inbox.org") "Arrangement:")
	;;  "* %? %^T")
	("t" "Todo")
	("tt" "Todo without time" entry (file+headline ,(concat org-directory "inbox.org") "Todo:")
	 "* SOMEDAY %?")
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
      '((sequence "TODO(t)" "SOMEDAY(s)" "CANCELED(c)" "|" "DONE(d)"))
      org-todo-keyword-faces
      '(("SOMEDAY" . "#34CCDB")
	("CANCELED" . "grey")))
;; set key
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\M-q" 'org-agenda)
;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
(defun ljg/org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
	   (background-dark-p (string= background "dark"))
	   (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
	   pos
	   duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
	(goto-char pos)
	(when (and (not (equal pos (point-at-eol)))
		   (setq duration (org-get-at-bol 'duration)))
	  (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
		(ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
	    (overlay-put ov 'face `(:background ,(car colors)
						:foreground
						,(if background-dark-p "black" "white")))
	    (setq colors (cdr colors))
	    (overlay-put ov 'line-height line-height)
	    (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'ljg/org-agenda-time-grid-spacing)

(when (eq system-type 'gnu/linux)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(package-selected-packages
     '(ein flycheck py-autopep8 elpy diminish ace-window org-bullets restart-emacs all-the-icons pdf-tools projectile undo-tree yasnippet company counsel youdao-dictionary highlight-parentheses org-download neotree which-key magit exec-path-from-shell use-package))
   '(show-paren-mode t)
   '(tool-bar-mode nil))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Purisa" :foundry "PfEd" :slant normal :weight bold :height 120 :width normal))))
   '(aw-leading-char-face ((t (:inderit ace-jump-face-foreground :height 3.0))))))
;; (font-get (face-attribute 'default :font) :family)
