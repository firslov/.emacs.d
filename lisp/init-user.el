;; init fullscreen
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ns-pop-up-frames nil)
;; desktop-save
;; (desktop-save-mode t)
;; (setq desktop-restore-in-current-display t)
;; (setq desktop-restore-frames t)
;; atom-one-dark theme
;; (load-theme 'atom-one-dark t)
(load-theme 'solarized-light-high-contrast t)
;; hide icon in titlebar
(setq ns-use-proxy-icon nil)
;; dashboard message
(setq dashboard-footer-messages
      '("So?"))
;; ui
(setq default-frame-alist
      (append (list
	       '(font . "MesloLGLDZ Nerd Font:style=Light:size=14")
	       ;; '(font . "Roboto Mono Emacs Regular:size=14")
	       ;; '(min-height . 1)  '(height     . 45)
	       ;; '(min-width  . 1) '(width      . 81)
	       ;; '(fullscreen . maximized)
	       '(left . (+ 20))
	       '(width . 167)
	       '(top . 40)
	       '(height . 35)
	       ;; '(bottom . 20)		
	       '(vertical-scroll-bars . nil)
	       '(internal-border-width . 24)
	       ;; '(left-fringe    . 0)
	       ;; '(right-fringe   . 0)
	       '(tool-bar-lines . 0)
	       '(menu-bar-lines . 0)
	       ;; 透明标题栏
	       '(ns-transparent-titlebar . t)
	       '(ns-appearance . dark)
	       )))
;; 置于default-frame-alist后，否则被覆盖
(awesome-tray-mode 1)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; 将函数 load-init-file 绑定到 <f1> 0 键上
(global-set-key (kbd "<f1> 0") 'load-init)
;; 将函数 open-init-file 绑定到 <f1> 1 键上
(global-set-key (kbd "<f1> 1") 'open-init-file)
;; 将函数 org-mind-conf 绑定到<f1> 2 键上
(global-set-key (kbd "<f1> 2") (lambda ()
				 (interactive)
				 (dired (concat user-emacs-directory "lisp/"))))
;; show startup page
(global-set-key (kbd "<f1> 3") 'show-startup-page)
;; 将函数 indent-buffer 绑定到 <f8> 键上
(global-set-key (kbd "<f8>") 'indent-buffer)
;; 上下翻半页
(global-set-key "\M-n" 'scroll-half-page-up)
(global-set-key "\M-p" 'scroll-half-page-down)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file (concat user-emacs-directory "readme.org")))
;; 快速加载配置文件
(defun load-init()
  (interactive)
  (org-babel-tangle-file "~/.emacs.d/readme.org")
  (load-file (concat user-emacs-directory "init.el")))
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
      (org-agenda nil "z"))))

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
;; (sanityinc/adjust-opacity nil -16)

(provide 'init-user)
