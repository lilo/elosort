;;; package --- elo.el -*- lexical-binding:t; coding:utf-8 -*-
;;; Commentary:
;;; TODO:
;;; Code:


(defun org-elo-compare-tabulated (item1 item2)
  "compare tabulated items by rating"
  (let* ((elo1 (string-to-number (car (aref (cadr item1) 0))))
         (elo2 (string-to-number (car (aref (cadr item2) 0)))))
    (< elo1 elo2)))

(define-derived-mode org-elo-list-top-mode tabulated-list-mode "Elo Top"
  "Elo Top"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("rating" 6 org-elo-compare-tabulated)
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
  (let ((elo-buf
         (get-buffer-create
          (format
           "*Org Elo Top for %s*"
           (buffer-name (get-buffer buf))))))
    (switch-to-buffer elo-buf)
    (org-elo-list-top-mode)
    (set (make-variable-buffer-local 'elo-source-buffer) buf)
    (setq-local elo-source-buffer buf)
    (org-elo-list-top-refresh)))

(defun org-elo-list-top-refresh ()
  "Refresh elo top."
  (let* ((entries
          (save-excursion
            (with-current-buffer elo-source-buffer
              (org-map-entries #'org-elo-tabulate)))))
  (setq tabulated-list-entries entries)
  (tabulated-list-init-header)
  (tabulated-list-print)))

(defun org-elo-tabulate ()
  "tabulate"
  (let-alist (org-elo-get-alist)
    (list
     nil ;; id
     (vector
      (list .elo)
      (list .title)
      (list .update)))))

(defun org-elo-get-alist ()
  (interactive)
  (let* ((id (org-entry-get (point) "ID" :inherit t))
         (elo (org-entry-get (point) "ELO" :inherit t))
         (title (org-entry-get (point) "ITEM"))
         (update (org-entry-get (point) "ELO_UPDATE" :inherit t))
         (fights (org-entry-get (point) "ELO_FIGHTS" :inherit t)))
    `((id . ,id)
      (elo . ,elo)
      (title . ,title)
      (update . ,update)
      (fights . ,fights))))
