;; cl - Common Lisp Extension
(require 'cl-lib)
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/"))))
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Add use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(add-to-list 'load-path "~/.emacs.d/git-repo/nano-emacs/")
(require 'nano)

;; (use-package phd
;;   :defer t)
(use-package org-elp
  :load-path "~/.emacs.d/git-repo/org-elp"
  :defer t)
(use-package shengci
  :ensure f
  :load-path "~/.emacs.d/git-repo/shengci.el")
(require 'org2ctex)
(org2ctex-toggle t)

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

(use-package good-scroll
  :load-path "~/.emacs.d/git-repo/good-scroll.el"
  :config
  (good-scroll-mode 1))

;; https://github.com/rlister/org-present
(add-to-list 'load-path "~/.emacs.d/git-repo/org-present")
(autoload 'org-present "org-present" nil t)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "○" "◆" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

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

(when (eq system-type 'windows-nt)
  (setq conf_dir "e:/emacs/.emacs.d/"
        org-directory "e:/org/"))

(when (eq system-type 'darwin)
  (setq conf_dir "~/.emacs.d/"
        org-directory "~/firslov/"))

(when (eq system-type 'gnu/linux)
  (setq conf_dir "~/.emacs.d/"
        org-directory "~/org/"))

;; init fullscreen
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-splash-screen t)

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
;; (defun init-my-frame ()
;;   (set-frame-position (selected-frame) 120 40)
;;   (set-frame-width (selected-frame) 128)
;;   (set-frame-height (selected-frame) 32))
;; (add-hook 'after-init-hook 'init-my-frame)
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

(advice-add 'my/show-todo :after (lambda (&rest r)
                                   (shrink-window-horizontally 12)))

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
;; (add-hook 'org-mode-hook 'linum-mode)
(setq org-agenda-files (directory-files org-directory t "\\.org$" t)
      lt-todo-files (directory-files org-directory t "\\.org$" t)
      org-image-actual-width '(400)
      org-agenda-skip-function-global '(org-agenda-skip-entry-if 'regexp "\\* DONE\\|\\* CANCELED")
      org-agenda-window-setup nil
      org-M-RET-may-split-line '((headline . nil))
      org-agenda-time-grid (quote
                            ((daily today require-timed remove-match)
                             (800 1800)
                             "......" "----------------"))
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

(setq org-capture-templates
      `(("i" "Inbox" entry (file+headline ,(concat org-directory "inbox.org") "Inbox:")
         "* %?" :unnarrowed t)
        ("j" "Journal" entry (file+datetree ,(concat org-directory "journal.org"))
         "* %U\n%?" :unnarrowed t)
        ("t" "Todo")
        ("tt" "Todo without time" entry (file+headline ,(concat org-directory "inbox.org") "Todo:")
         "* SOMEDAY %?")
        ("ts" "Todo with SCHEDULED" entry (file+headline ,(concat org-directory "inbox.org") "Todo:")
         "* TODO %?\nSCHEDULED:%^t")
        ("td" "Todo with DEADLINE" entry (file+headline ,(concat org-directory "inbox.org") "Todo:")
         "* TODO %?\nDEADLINE:%^t")))

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
