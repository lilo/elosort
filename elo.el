;;; package --- elo.el -*- lexical-binding:t; coding:utf-8 -*-
;;; Commentary:
;;; Code:

(defcustom org-elo-k 20
  "The K."
  :group 'elo)

(defcustom org-elo-starting-elo 1200
  "Starting elo."
  :group 'elo)

(defcustom org-elo-file nil
  "File with the list of contenders."
  :type '(file :must-match t)
  :group 'elo)

(defun org-elo-compute-elo (winner loser) ; TODO: incorrect update
  "Compute new elo for WINNER and LOSER using K."
  (let* ((k org-elo-k)
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
         (fights (org-entry-get-multivalued-property pom "ELO_FIGHTS")) ; TODO: plist (p1-id num-p1-p2-games ...)
         (num-fights (if (consp fights)
                         (length fights)
                       0)))
    `((id . ,id)
      (elo . ,(if (stringp elo) (string-to-number elo) org-elo-starting-elo))
      (title . ,title)
      (fights . ,fights)
      (num-fights . ,num-fights))))

(defun org-elo-compare-tabulated (item1 item2)
  "Compare tabulated items by rating"
  (let ((elo1 (pcase item1 (`(,id ,_) (let-alist id .elo))))
        (elo2 (pcase item2 (`(,id ,_) (let-alist id .elo)))))
    (< elo1 elo2)))

(define-derived-mode org-elo-list-top-mode tabulated-list-mode "Elo Top"
  "Elo Top"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("rating" 8 org-elo-compare-tabulated)
			       ("title" 50 t)
                               ("num_games" 16 t)])
  (setq tabulated-list-sort-key
        (cons "rating" :inverted))
  (add-hook 'tabulated-list-revert-hook #'org-elo-list-top-refresh nil t))

(put 'org-elo-list-top-mode 'mode-class 'special)

(defun org-elo-list-top ()
  "Display the score board."
  (interactive)
  (let ((elo-buf
         (get-buffer-create
           "*Org Elo Top")))
    (switch-to-buffer elo-buf)
    (org-elo-list-top-mode)
    (setq-local elo-source-buffer elo-buf)
    (org-elo-list-top-refresh)))

(defun org-elo-list-top-refresh ()
  "Refresh elo top."
  (let* ((entries
          (save-excursion
            (with-current-buffer (find-file-noselect org-elo-file)
              (org-map-entries
               (lambda () (org-elo-tabulate (org-elo-get-alist))))))))
    (setq tabulated-list-entries entries)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defvar org-elo-list-top-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'org-elo-list-top-refresh)
    (define-key map (kbd "f") #'org-elo-fight)
    map)
  "Org-elo scoreboard mode keymap.")

(defun org-elo-tabulate (alist)
  "Item alist as tabulated-list entry."
  (let-alist alist
    (list
     alist
     (vector
      (list (number-to-string .elo))
      (list .title)
      (list (number-to-string .num-fights))))))


(defvar org-elo-fight-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "RET") #'org-elo-fight-win1)
    (define-key map (kbd ",") #'org-elo-fight-win1)
    (define-key map (kbd ".") #'org-elo-fight-win2)
    (define-key map (kbd "g") #'org-elo-fight-revert)
    (define-key map (kbd "t") #'org-elo-list-top)
    map)
  "Fight mode keymap.")

(defun org-elo-fight-win1 ()
  "Set p1 as winner and update"
  (interactive)
  (org-elo-fight-update t))

(defun org-elo-fight-win2 ()
  "Set p2 as winner and update"
  (interactive)
  (org-elo-fight-update nil))

(defun org-elo-fight-update (p1-winner-p)
  "Update records for current pair.
Set elo."
  (let* ((p1 org-elo-p1)
         (p2 org-elo-p2)
         (p1-id (let-alist p1 .id))
         (p2-id (let-alist p2 .id))
         (p1-title (let-alist p1 .title))
         (p2-title (let-alist p2 .title))
         (p1-fights (let-alist p1 .fights))
         (p2-fights (let-alist p2 .fights))
         (p1-elo (or (let-alist p1 .elo) org-elo-starting-elo))
         (p2-elo (or (let-alist p2 .elo) org-elo-starting-elo))
         (new-elos (if
                       p1-winner-p
                       (org-elo-compute-elo p1-elo p2-elo)
                     (org-elo-compute-elo p2-elo p1-elo)))
         (p1-new-elo (if p1-winner-p (car new-elos) (cdr new-elos)))
         (p2-new-elo (if p1-winner-p (cdr new-elos) (car new-elos))))
    (save-excursion
      (with-current-buffer (find-file-noselect org-elo-file)
        (let* ((p1pom (org-id-find p1-id :marker))
               (p2pom (org-id-find p2-id :marker)))
          (org-entry-put p1pom "ELO" (number-to-string p1-new-elo))
          (org-entry-put p1pom "ELO_FIGHTS" (if p1-fights
                                                (mapconcat 'identity (append p1-fights (list p2-id)) " ")
                                              p2-id))
          (org-entry-put p2pom "ELO" (number-to-string p2-new-elo))
          (org-entry-put p2pom "ELO_FIGHTS" (if p2-fights
                                                (mapconcat 'identity (append p2-fights (list p1-id)) " ")
                                              p1-id))
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

(defun org-elo-next-pair (items)
  "Return (cons item1 item2) from items list."
  (let* ((entries (copy-sequence items))
         (sorted
          (sort entries
                (lambda (a b)
                  (< (let-alist a .num-fights)
                     (let-alist b .num-fights)))))
         (p1 (car sorted))
         (p1-id (let-alist p1 .id))
         (but-p1 (cdr sorted))
         (p2 (car (sort but-p1
                        (lambda (a b)
                          (let* ((afs (let-alist a
                                       (cl-count
                                        p1-id .fights :test #'string-equal)))
                                 (bfs (let-alist b
                                        (cl-count
                                         p1-id .fights :test #'string-equal))))
                            (< afs bfs)))))))
    (cons p1 p2)))

(defun org-elo-fight-revert ()
  "Refresh buffer, get next candidates."
  (interactive)
  (save-excursion
    (with-current-buffer (find-file-noselect org-elo-file)
      (setq entries (shuffle (org-map-entries #'org-elo-get-alist)))))
  (let* ((inhibit-read-only t)
         (pair (org-elo-next-pair entries))
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
    ; (insert (format "Is %s > %s?\n" p1-title p2-title))
    (insert-button p1-title 'action (lambda (_) (org-elo-fight-win1)))
    (insert " vs ")
    (insert-button p2-title 'action (lambda (_) (org-elo-fight-win2)))))

(defun org-elo-fight ()
  "fight."
  (interactive)
  (let ((fight-buf
         (get-buffer-create
           "*Org Elo fight")))
    (pop-to-buffer-same-window fight-buf)
    (with-current-buffer fight-buf
      (setq inhibit-read-only t)      
      (org-elo-fight-mode)
      (setq-local org-elo-buf fight-buf)
      (org-elo-fight-revert))))

(provide 'org-elo)
