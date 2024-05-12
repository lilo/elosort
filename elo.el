;;; package --- elo.el -*- lexical-binding:t; coding:utf-8 -*-
;;; Commentary:
;;; - 
;;; TODO:
;;; round elo to int
;;; customization of the list of items
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
         (id (org-entry-get pom "ID"))
         (elo (org-entry-get pom "ELO"))
         (title (org-entry-get pom "ITEM"))
         (update (org-entry-get pom "ELO_UPDATE"))
         (fights (org-entry-get-multivalued-property pom "ELO_FIGHTS"))
         (num-fights (if (consp fights)
                         (length fights)
                       0)))
    `((id . ,id)
      (elo . ,(when (stringp elo) (string-to-number elo)))
      (title . ,title)
      (update . ,update)
      (fights . ,fights)
      (num-fights . ,num-fights))))

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
        (list (number-to-string .elo))
        (list .title)
        (list .update))))))


(defvar org-elo-fight-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "RET") #'org-elo-fight-win1)
    (define-key map (kbd ",") #'org-elo-fight-win1)
    (define-key map (kbd ".") #'org-elo-fight-win2)
    (define-key map (kbd "g") #'org-elo-fight-revert)
    map)
  "Fight mode keymap.")

(defun org-elo-fight-win1 ()
  "Set p1 as winner and update"
  (interactive)
  (org-elo-fight-update t))

(defun org-elo-fight-win2 ()
  "Set p1 as winner and update"
  (interactive)
  (org-elo-fight-update nil))

(defun org-elo-fight-update (&optional p1-winner-p)
  "Update records for current pair.
Set elo and elo_update."
  (interactive)
  (let* ((p1 org-elo-p1)
         (p2 org-elo-p2)
         (p1-id (let-alist p1 .id))
         (p2-id (let-alist p2 .id))
         (p1-elo (or (let-alist p1 .elo) org-elo-starting-elo))
         (p2-elo (or (let-alist p2 .elo) org-elo-starting-elo))
         (new-elos (if
                       p1-winner-p
                       (org-elo-compute-elo p1-elo p2-elo)
                     (org-elo-compute-elo p2-elo p1-elo)))
         (p1-new-elo (car new-elos))
         (p2-new-elo (cdr new-elos))
         (date-str (format-time-string "%Y-%m-%d")))
    (save-excursion
      (with-current-buffer org-elo-buf
        (let* ((p1pom (org-id-find p1-id :marker))
               (p2pom (org-id-find p2-id :marker)))
          (org-entry-put p1pom "ELO" (number-to-string p1-new-elo))
          (org-entry-put p1pom "ELO_UPDATE" date-str)
          (org-entry-add-to-multivalued-property p1pom "ELO_FIGHTS" p2-id)
          (org-entry-put p2pom "ELO" (number-to-string p2-new-elo))
          (org-entry-put p2pom "ELO_UPDATE" date-str)
          (org-entry-add-to-multivalued-property p2pom "ELO_FIGHTS" p1-id)
          (save-buffer)))))
  (org-elo-fight-revert))

(define-derived-mode org-elo-fight-mode special-mode "org-elo-fight-mode"
  "Fight!"
  (setq buffer-read-only t)
  (defvar-local org-elo-buf nil)
  (defvar-local org-elo-p1 nil "Candidate #1")
  (defvar-local org-elo-p2 nil "Candidate #2"))

(put 'org-elo-fight-mode 'mode-class 'special)




;;;
(defun swap (LIST el1 el2)
  "in LIST swap indices EL1 and EL2 in place"
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))


(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (cl-loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1))))
             (swap LIST i j)))
  LIST)

(defun org-elo-next-pair (buf)
  "Return (cons item1 item2) from BUF."
  (with-current-buffer buf
    (let* ((entries (shuffle (org-map-entries #'org-elo-get-alist)))
           (sorted-entries
            (sort
             (copy-sequence entries)
             (lambda (p1 p2)
               (let ((p1-update (let-alist p1 .update))
                     (p1-num (or (let-alist p1 .num-fights) 0))
                     (p2-update (let-alist p2 .update))
                     (p2-num (or (let-alist p2 .num-fights) 0)))
                 (or (< p1-num p2-num)
                     (when (= p1-num p2-num)
                       (string-lessp p1-update p2-update))))))))
      (seq-let (p1 p2 _) sorted-entries
        (cons p1 p2)))))

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
