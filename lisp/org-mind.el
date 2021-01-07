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

;; org-roam
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/firslov")
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-graph)
	       ("C-c n t" . org-tags-view))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate)))
  :config
  (setq org-roam-tag-sources '(prop last-directory)
	org-roam-capture-templates
	'(("d" "default" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "${slug}"
	   :head "#+title: ${title}\n"
	   :unnarrowed t)
	  ;; ("t" "tag" plain (function org-roam--capture-get-point)
	  ;;  "%?"
	  ;;  :file-name "tag/${slug}"
	  ;;  :head "#+title: ${title}\n"
	  ;;  :unnarrowed t)
	  ;; ("j" "journal" plain (function org-roam--capture-get-point)
	  ;;  "%?"
	  ;;  :file-name "journal/${title}"
	  ;;  :head "#+title: ${title}\n"
	  ;;  :unnarrowed t)
	  )))

;; org-capture-journal
(defun my/org-capture-journal ()
  (interactive)
  "Capture a journal."
  (org-capture nil "j"))
(define-key global-map (kbd "C-c n j") 'my/org-capture-journal)

;; org-journal
;; (use-package org-journal
;;   :ensure t
;;   :bind ("C-c n j" . org-journal-new-entry)
;;   :config
;;   (setq org-journal-dir "~/firslov"
;; 	org-journal-file-type 'yearly
;; 	org-journal-date-format "%Y-%m-%d %A"
;; 	org-journal-file-format "journal_%Y.org"))

;; helm-org-rifle
(use-package helm-org-rifle
  :ensure t
  :bind (("C-c n r" . org-roam-rifle))
  :config
  (defun org-roam-rifle ()
    "Use roam directory rifle"
    (interactive)
    (helm-org-rifle-directories org-roam-directory))

  (defun my/helm-org-rifle--store-link (candidate)
    "Store link into CANDIDATE."
    (-let (((buffer . pos) candidate))
      (with-current-buffer buffer
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
  :config (setq deft-directory "~/firslov"
		deft-extensions '("md" "org")
		;; deft-recursive t
		deft-use-filename-as-title t))

;; super-agenda
(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
	'(;; Each group has an implicit boolean OR operator between its selectors.
	  (:name "Today"  ; Optionally specify section name
		 :time-grid t  ; Items that appear on the time grid
		 :todo "TODAY")  ; Items that have this TODO keyword
	  (:name "Urgent"
		 :deadline (past today))
	  (:name "Important"
		 ;; Single arguments given alone
		 :priority>= "B")
	  (:name "Habits"  ; Optionally specify section name
		 :habit t)))
  (org-super-agenda-mode))

;; org-ql
(use-package org-ql
  :ensure t
  :config
  (defun my/show-todo ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo) (not (todo "CANCELED")))
      :super-groups '(;; Each group has an implicit boolean OR operator between its selectors.
		      (:name "Urgent"
			     :and (:deadline (today)     
					     :not (:habit t))
			     :and (:deadline (past)
					     :not (:habit t))
			     :and (:scheduled (past)
					      :not (:habit t)))
		      (:name "Today"  ; Optionally specify section name
			     :scheduled (today))  ; Items that have this TODO keyword
		      (:name "Important"
			     ;; Single arguments given alone
			     :priority>= "B")
		      (:name "Habits"  ; Optionally specify section name
			     :habit t)
		      (:name "Someday"
			     :todo "SOMEDAY"))
      ))
  (global-set-key (kbd "<f1> 3") 'my/show-todo))
(use-package org-ql-view
  :config
  ;; (define-key org-ql-view-map "q" #'delete-window)
  )

;; emacs-calfw
(use-package calfw-org
  :ensure calfw
  :bind (("C-c n C" . cfw:open-org-calendar)
	 ("C-c n c" . toggle-calendar))
  :config
  (defun toggle-calendar ()
    (interactive)
    (if (and (get-buffer-window "*Calendar*" 'visible) (gnus-buffer-exists-p "*Calendar*"))
	(calendar-exit)
      (calendar))))

;; misc
(use-package org-sidebar
  :ensure t)
(use-package org-download
  :ensure t
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "./src")
  (setq org-download-display-inline-images nil))
(use-package valign
  :load-path "~/.emacs.d/git-repo/valign"
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(provide 'org-mind)
