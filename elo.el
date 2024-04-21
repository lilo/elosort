;;; package --- elo.el -*- lexical-binding:t; coding:utf-8 -*-
;;; Commentary:
;;; - 
;;; TODO:
;;; round elo to int
;;; Code:

(defcustom org-elo-k 20
  "The K.")

(defcustom org-elo-starting-elo 1200
  "Starting elo.")

(defun org-elo-compute-elo (winner loser &optional k)
  "Compute new elo for WINNER and LOSER using K."
  (let* ((k (or k org-elo-k))
         (p1 (/ 1.0 (+ 1.0 (expt 10 (/ (- loser winner) 400.0)))))
         (w (+ winner (* k (- 1 p1))))
         (p2 (/ 1.0 (+ 1.0 (expt 10 (/ (- winner loser) 400.0)))))
         (l (+ loser (* k (- 0 p2)))))
    (cons w l)))

(defun org-elo-get-alist ()
  "Current entry as alist."
  (interactive)
  (let* ((pom (point))
         (id (org-entry-get (pom) "ID"))
         (elo (org-entry-get (pom) "ELO"))
         (title (org-entry-get (pom) "ITEM"))
         (update (org-entry-get (pom) "ELO_UPDATE"))
         (fights (org-entry-get (pom) "ELO_FIGHTS")))
    `((id . ,id)
      (elo . ,elo)
      (title . ,title)
      (update . ,update)
      (fights . ,fights))))

(defun org-elo-compare-tabulated (item1 item2)
  "Compare tabulated items by rating"
  (let* ((elo1 (string-to-number (car (aref (cadr item1) 0))))
         (elo2 (string-to-number (car (aref (cadr item2) 0)))))
    (< elo1 elo2)))

(define-derived-mode org-elo-list-top-mode tabulated-list-mode "Elo Top"
  "Elo Top"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("rating" 8 org-elo-compare-tabulated)
			       ("title" 50 t)
                               ("updated" 16 t)])
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
  "Item alist as tabulated-list entry."
  (let ((alist (org-elo-get-alist)))
    (let-alist alist
      (list
       alist
       (vector
        (list .elo)
        (list .title)
        (list .update))))))


(defvar org-elo-fight-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-elo-fight-win1)
    (define-key map (kbd ",") #'org-elo-fight-win1)
    (define-key map (kbd ".") #'org-elo-fight-win2)
    (define-key map (kbd "g") #'org-elo-fight-revert)
    map)
  "Fight mode keymap.")

(defun org-elo-fight-win2 ()
  (interactive)
  (message "win2"))

(defun org-elo-fight-win1 ()
  (interactive)
  (let* ((p1-elo )))
  (progn
    ;; compute ratings for both
    ;; set new ratings for both
    ;; update date of last fight
    ;; append opponent's id to the list of fights
    ))

(define-derived-mode org-elo-fight-mode special-mode "org-elo-fight-mode"
  "Fight!"
  (setq buffer-read-only t)
  (defvar-local org-elo-buf nil)
  (defvar-local org-elo-p1 nil "Candidate #1")
  (defvar-local org-elo-p2 nil "Candidate #2"))

(put 'org-elo-fight-mode 'mode-class 'special)

(defun org-elo-next-pair (buf)
  "Return (cons item1 item2) from BUF."
  (with-current-buffer buf
    (let* ((entries (org-map-entries #'org-elo-get-alist))
           (p1 (seq-random-elt entries))
           (p2 (seq-random-elt entries)))
      (cons p1 p2))))

(defun org-elo-fight-revert ()
  "Refresh buffer, get next candidates."
  (interactive)
  (let* ((inhibit-read-only t)
         (pair (org-elo-next-pair org-elo-buf))
         (p1 (car pair))
         (p1-id (let-alist p1 .id))
         (p1-elo (let-alist p1 .elo))
         (p1-title (let-alist p1 .title))
         (p2 (cdr pair))
         (p2-id (let-alist p2 .id))
         (p2-elo (let-alist p2 .elo))
         (p2-title (let-alist p2 .title)))
    (setq-local org-elo-p1 p1)
    (setq-local org-elo-p2 p2)
    (delete-region (point-min) (point-max))
    (insert (format "Is %s > %s?\n" p1-title p2-title))))

(defun org-elo-fight (buf)
  "Display elo fight for BUF."
  (interactive "b")
  (let ((inhibit-read-only t)
        (fight-buf
         (get-buffer-create
          (format
           "*Org Elo fight %s*"
           (buffer-name (get-buffer buf))))))
    (pop-to-buffer-same-window fight-buf)
    (with-current-buffer fight-buf
      (org-elo-fight-mode)
      (set (make-variable-buffer-local 'org-elo-buf) buf)
      (setq-local org-elo-buf buf)
      (org-elo-fight-revert))))

(provide 'org-elo)
