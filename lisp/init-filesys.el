;; org
(use-package org
  :ensure t)

;; habits
(add-to-list 'org-modules 'org-habit t)
;; I prefer to log TODO creation also
(setq org-treat-insert-todo-heading-as-state-change t)
;; log into LOGBOOK drawer
(setq org-log-into-drawer t)
;; variable
(setq org-habit-show-all-today t)

;; journal
(defun journal ()
  (interactive)
  (org-capture nil "j"))

;; key-bindings
(global-set-key (kbd "C-c n c") 'org-capture)
(global-set-key (kbd "C-c n j") 'journal)
(global-set-key (kbd "C-c n s") 'my/global-search)


(defun my/global-search (arg)
  (interactive "sRgrep search for: ")
  (grep-compute-defaults)
  (rgrep arg "*.org" org-directory nil))

;; helm-org-rifle
(use-package helm-org-rifle
  :ensure t
  :bind (("C-c n r" . my/org-rifle))
  :config
  (defun my/org-rifle ()
    "Use org directory rifle"
    (interactive)
    (helm-org-rifle-directories org-directory))

  (defun my/helm-org-rifle--store-link (candidate)
    "Store link into CANDIDATE."
    (-let (((buffer . pos) candidate)) 
      (with-current-buffer  (find-file-noselect (buffer-file-name buffer))
	(goto-char pos)
	(call-interactively 'org-store-link))))

  (defun my/helm-org-rifle--insert-link (candidate)
    "Insert link to CANDIDATE in current location."
    (interactive)
    (my/helm-org-rifle--store-link candidate)
    (call-interactively 'org-insert-link))

  ;; add new actions to the default rifle action list
  (setq helm-org-rifle-actions
	(append helm-org-rifle-actions
		(helm-make-actions
		 "Store link" 'my/helm-org-rifle--store-link
		 "Insert link" 'my/helm-org-rifle--insert-link))))

;; deft
(use-package deft
  :ensure t
  :bind ("C-c n d" . deft)
  :commands (deft)
  :custom (deft-directory org-directory)
  :config (setq deft-extensions '("md" "org")
		deft-auto-save-interval 0
		deft-recursive nil
		deft-use-filename-as-title t
		deft-use-filter-string-for-filename t
		deft-default-extension "org"
		deft-org-mode-title-prefix t))


;; super-agenda
(use-package org-super-agenda
  :ensure t
  :config
  (setq org-agenda-custom-commands
	'(("z" "Firslov view"
	   ((agenda "" ((org-agenda-span 'day)
			(org-super-agenda-groups
			 '((:name "Today"  ; Optionally specify section name
				  :time-grid t  ; Items that appear on the time grid
				  :todo "TODAY")  ; Items that have this TODO keyword
			   (:name "Habits"
				  :habit t)))))
	    (alltodo "" ((org-agenda-overriding-header "")
			 (org-super-agenda-groups
			  '((:name "Next to do"
				   :todo "NEXT"
				   :order 1)
			    (:name "Urgent"
				   :deadline today
				   :order 2)
			    (:name "Important"
				   :tag "Important"
				   :priority>= "B"
				   :order 3)
			    (:name "Due Soon"
				   :deadline future
				   :order 8)
			    (:name "Overdue"
				   :deadline past
				   :order 7)
			    (:name "Phd"
				   :tag "phd"
				   :order 15)
			    (:name "Habits"
				   :habit t
				   :order 80)
			    (:name "Unimportant"
				   :priority<= "C"
				   :todo ("SOMEDAY")
				   :order 90)
			    (:discard (:tag ("Routine" "Daily")))))))))))
  (org-super-agenda-mode))

;; misc
(use-package org-appear
  :load-path "~/.emacs.d/git-repo/org-appear"
  :config
  (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-appear-autolinks t))
(use-package org-sidebar
  :ensure t)
(use-package org-download
  :ensure t
  :config
  (defun org-download--dir-2 ()
    "Return the current filename instead of heading name"
    (file-name-base (buffer-file-name)))
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir (concat org-directory "/src"))
  (setq org-download-display-inline-images nil))
(use-package valign
  :load-path "~/.emacs.d/git-repo/valign"
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(provide 'init-filesys)
