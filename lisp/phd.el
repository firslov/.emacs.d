(defgroup phd nil
  "Something about phd."
  :group 'phd)
(defcustom unread-dir "~/Desktop/Article/Downloads/"
  "Save downloads."
  :type 'string
  :group 'phd)
(defcustom archive-dir "~/Desktop/Article/Archive/"
  "Save articles."
  :type 'string
  :group 'phd)

;; work with interleave-mode

;;;###autoload
(defun mv-articles ()
  "Move articles from downloads to archive."
  (interactive)
  (let* ((unread-articles (cdr (cdr (directory-files unread-dir)))))
    (when unread-articles
      (dolist (article unread-articles)
	(when (string-match ".pdf" article)
	  (eshell-command (format "mv %s %s" (concat unread-dir article) (concat archive-dir article)))
	  (save-excursion
	    (goto-line (+ 1 (org-find-exact-headline-in-buffer "Unread")))
	    (insert (format "** [[%s][%s]]\n" (concat archive-dir article) article))))
	))))

;; (defun mv-articles ()
;;   (interactive)
;;   (let* ((unread-articles (cdr (cdr (directory-files unread-dir)))))
;;     (when unread-articles
;;       (dolist (article unread-articles)
;; 	(when (string-match ".pdf" article)
;; 	  (eshell-command (format "mv %s %s" (concat unread-dir article) (concat archive-dir article)))
;; 	  (save-excursion
;; 	    (end-of-buffer)
;; 	    (insert (format "** [[file:%s][%s]]\n" (concat archive-dir article) article))))
;; 	))))

(defun if-run-mv ()
  (when (equal (buffer-name) "Archive.org")
    (mv-articles)))

(add-hook 'org-mode-hook 'if-run-mv)

(provide 'phd)

