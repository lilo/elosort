;; for headings in current org file
;; properties: elo-games, elo-rating, elo-last
;; elo-tournament-judge (&optional N)
;; elo-list-top
;; 
;; 

(require 'org-ql)

(define-minor-mode org-elo-mode "org elo mode"
  :lighter " Э") ;; CYRILLIC CAPITAL LETTER E

(define-derived-mode org-elo-list-top-mode tabulated-list-mode "Elo Top"
  "Elo Top"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("rating" 6 t)
			       ("title" 28 t)
                               ("updated" 12 t)])
  (setq tabulated-list-sort-key
        (cons
         "rating"
         t)) ;; inverted
  (add-hook 'tabulated-list-revert-hook #'org-elo-list-top-refresh nil t))

(put 'org-elo-list-top-mode 'mode-class 'special)

(defun org-elo-list-top (buf)
  "Display elo top for BUF."
  (interactive "b")
  (let ((source-buffer buf)
        (elo-buf (get-buffer-create "*Org Elo Top*")))
    (switch-to-buffer elo-buf)
    (with-current-buffer elo-buf ;; TODO:
      (org-elo-list-top-mode)
      (set (make-variable-buffer-local 'elo-source-buffer) buf)
      (setq-local elo-source-buffer buf)
      (org-elo-list-top-refresh))))

(defun org-elo-list-top-refresh ()
  "Refresh elo top."
  (let* ((src-buf
         (buffer-local-value
          'elo-source-buffer
          (current-buffer)))
        (entries
         (save-excursion
           (with-current-buffer src-buf
             (org-map-entries #'org-elo-tabulate)))))
  (setq tabulated-list-entries entries)
  (tabulated-list-init-header)
  (tabulated-list-print)))

(defun org-elo-tabulate ()
  "tabulate"
  (let-alist (org-elo-get-elo)
    (list
     nil ;; id
     (vector
      (list .elo)
      (list .title)
      (list .update)))))

(defun org-elo-get-elo ()
  (interactive)
  (let* ((elo (org-entry-get (point) "ELO" :inherit t))
         (title (org-entry-get (point) "ITEM"))
         (update (org-entry-get (point) "ELO_UPDATE" :inherit t)))
    `((elo . ,elo)
      (title . ,title)
      (update . ,update))))
