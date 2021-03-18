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

;; writing
(defun writing ()
    (interactive)
  (org-capture nil "w"))

;; org-roam
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory org-directory)
  :bind (("C-c n c" . org-capture)
         ("C-c n s" . roam-global-search)
	 ("C-c n w" . writing)
         :map org-roam-mode-map
	 (("C-c n l" . org-roam)
	  ("C-c n f" . org-roam-find-file)
	  ("C-c n g" . org-roam-graph))
	 :map org-mode-map
	 (("C-c n i" . org-roam-insert))
	 (("C-c n I" . org-roam-insert-immediate)))
  :config
  (setq org-roam-tag-sources '(prop last-directory)
        org-roam-title-sources '(title alias)
        org-roam-rename-file-on-title-change nil
	org-roam-capture-templates
	'(("d" "default" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "${slug}"
	   :head "#+title: ${title}\n"
	   :unnarrowed t)
          ("b" "base" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "base/${slug}"
	   :head "#+title: ${title}\n"
	   :unnarrowed t)
          ("p" "paper" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "science/${slug}"
           :head "#+title: ${title}\n#+author: \n#+year: \n#+journal: \n#+date: %<%Y-%m-%d>\n#+roam_key: \n#+setupfile: config.setup\n\nbibliography:phd.bib"
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
	  ))
  (use-package org-roam-bibtex
    :ensure ivy-bibtex
    :hook (org-roam-mode . org-roam-bibtex-mode)))

;; org-journal
;; (use-package org-journal
;;   :ensure t
;;   :bind ("C-c n j" . org-journal-new-entry)
;;   :config
;;   (setq org-journal-dir "~/firslov"
;; 	org-journal-file-type 'yearly
;; 	org-journal-date-format "%Y-%m-%d %A"
;; 	org-journal-file-format "journal_%Y.org"))

(defun roam-global-search (arg)
  (interactive "sRgrep search for: ")
  (grep-compute-defaults)
  (rgrep arg "*.org" org-roam-directory nil))

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
  :init (setq deft-directory org-roam-directory)
  :config (setq deft-extensions '("md" "org")
                deft-auto-save-interval 0
		deft-recursive t
		deft-use-filename-as-title t
                deft-use-filter-string-for-filename t
                deft-default-extension "org"
                deft-org-mode-title-prefix t))

;; org-ref
(use-package org-ref
  :init (require 'helm-swoop)
  :config
  (setq org-latex-prefer-user-labels t))

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
  (setq-default org-download-image-dir (concat org-roam-directory "/src"))
  (setq org-download-display-inline-images nil))
(use-package valign
  :load-path "~/.emacs.d/git-repo/valign"
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(provide 'init-roam)
