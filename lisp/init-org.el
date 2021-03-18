;; org variables
;; (add-to-list 'org-file-apps '("\\.pdf\\'" . "Microsoft\ edge %s"))
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
;; (add-hook 'org-mode-hook 'linum-mode)
(setq org-agenda-files (list (concat org-directory "/inbox.org") (concat org-directory "/journal.org"))
      lt-todo-files (list (concat org-directory "/inbox.org") (concat org-directory "/journal.org"))
      org-image-actual-width '(400)
      org-agenda-skip-function-global '(org-agenda-skip-entry-if 'regexp "\\* DONE\\|\\* CANCELED")
      org-agenda-window-setup nil
      org-deadline-warning-days 14
      org-M-RET-may-split-line '((headline . nil))
      org-use-tag-inheritance t
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
      `(("i" "Inbox" entry (file+headline ,(concat org-directory "/inbox.org") "Inbox:")
	 "* %?" :unnarrowed t)
	("b" "Bibtex")
	("br" "references" plain (file ,(concat org-roam-directory "/references.bib")))
	("bo" "phd" plain (file ,(concat org-roam-directory "/phd.bib")))
	("j" "Journal" entry (file+datetree ,(concat org-directory "/journal.org"))
	 "* %U\n%?" :unnarrowed t)
	("w" "Writing" entry (file+datetree ,(concat org-directory "/write.org"))
	 "* %U\n%?" )
	("t" "Todo")
	("tt" "Todo without time" entry (file+headline ,(concat org-directory "/inbox.org") "Todo:")
	 "* SOMEDAY %?")
	("ts" "Todo with SCHEDULED" entry (file+headline ,(concat org-directory "/inbox.org") "Todo:")
	 "* TODO %?\nSCHEDULED:%^t")
	("td" "Todo with DEADLINE" entry (file+headline ,(concat org-directory "/inbox.org") "Todo:")
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

(provide 'init-org)
