;;; elosort.el --- Sort a list of incomparable things  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Sergey Lilo
;; Author: Sergey Lilo <s3rg31110@gmail.com>
;; Created: 14 Apr 2024
;; Keywords: games
;; URL: https://github.com/lilo/elosort.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; 1. Prepare file with list of things in org-mode format:
;; #+elosort_question: What is tastier?
;; * apples
;; * oranges
;; * pears
;; * ...
;;
;; Run elosort with {M-x elosort-fight RET} and select the file.
;;
;; You can learn more about Elo here:
;; https://en.wikipedia.org/wiki/Elo_rating_system
;;

;;; Code:

(defcustom elosort-k 20
  "The K."
  :group 'elo)

(defcustom elosort-starting-elo 1200
  "Starting elo."
  :group 'elo)

(defvar elosort-file nil
  "The DB file.")

(defun elosort-get-question ()
  "Return property `elosort_question' from the file."
  (when-let ((elosort-question (alist-get
   "ELOSORT_QUESTION"
   (org-collect-keywords '("elosort_question"))
   nil nil #'equal)))
    (car elosort-question)))

(defun elosort-get-by-title (title)
  "Return POM of the heading with matching TITLE."
  (cl-dolist (pt (org-map-entries #'point))
    (when
        (string-equal
         title
         (alist-get
          "ITEM"
          (org-entry-properties pt)
          nil
          nil
          #'string-equal))
      (cl-return pt))))

(defun elosort-compute-elo (winner loser)
  "Compute new elo for WINNER and LOSER using K."
  (let* ((k elosort-k)
         (p1 (/ 1.0 (+ 1.0 (expt 10 (/ (- loser winner) 400.0)))))
         (w (+ winner (* k (- 1 p1))))
         (p2 (/ 1.0 (+ 1.0 (expt 10 (/ (- winner loser) 400.0)))))
         (l (+ loser (* k (- 0 p2)))))
    (cons (round w) (round l))))

(defun elosort-get-alist ()
  "Current entry as alist."
  (interactive)
  (let* ((pom (point))
         (elo (org-entry-get pom "ELO"))
         (title (org-entry-get pom "ITEM"))
         (plist-fights
          (org-entry-get-multivalued-property pom "ELO_FIGHTS"))
         (plist-numfights ;; sum values in plist
          (cl-loop
           for n from 1 to (length plist-fights) by 2
           sum (string-to-number (nth n plist-fights)))))
    `((elo . ,(if (stringp elo) (string-to-number elo) elosort-starting-elo))
      (title . ,title)
      (fights . ,plist-fights)
      (num-fights . ,plist-numfights))))


(defun elosort-compare-tabulated (item1 item2)
  "Compare tabulated items by rating"
  (let ((elo1
         (pcase
             item1
           (`(,tabulated-id ,_)
            (let-alist tabulated-id .elo))))
        (elo2
         (pcase
             item2
           (`(,tabulated-id ,_)
            (let-alist tabulated-id .elo)))))
    (< elo1 elo2)))

(defvar elosort-list-top-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'elosort-fight-from-top)
    map)
  "elosort scoreboard mode keymap.")

(define-derived-mode elosort-list-top-mode tabulated-list-mode "elosort-list-top-mode"
  "Elo Top"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("rating" 8 elosort-compare-tabulated)
			       ("title" 50 t)
                               ("num_games" 16 t)])
  (setq tabulated-list-sort-key
        (cons "rating" :inverted))
  (add-hook 'tabulated-list-revert-hook #'elosort-list-top-refresh nil t))

(put 'elosort-list-top-mode 'mode-class 'special)

(defun elosort-list-top (filename)
  "Display the score board."
  (interactive "fSelect file: ")
  (let ((elo-buf
         (get-buffer-create
           "*Org Elo Top*")))
    (switch-to-buffer elo-buf)
    (elosort-list-top-mode)
    (setq-local elo-source-buffer elo-buf)
    (setq elosort-file filename)
    (elosort-list-top-refresh)))

(defun elosort-list-top-refresh ()
  "Refresh elo top."
  (interactive)
  (let* ((entries
          (save-excursion
            (with-current-buffer (find-file-noselect elosort-file)
              (org-map-entries
               (lambda () (elosort-tabulate (elosort-get-alist))))))))
    (setq tabulated-list-entries entries)
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun elosort-tabulate (alist)
  "Item alist as tabulated-list entry."
  (let-alist alist
    (list
     alist
     (vector
      (list (number-to-string .elo))
      (list .title)
      (list (number-to-string .num-fights))))))

(defun elosort-list-top-from-fight ()
  "Call from fight mode providing `elosort-file' as argument"
  (interactive)
  (elosort-list-top elosort-file))


(defvar elosort-fight-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ",") #'elosort-fight-win1)
    (define-key map (kbd ".") #'elosort-fight-win2)
    (define-key map (kbd "g") #'elosort-fight-revert)
    (define-key map (kbd "t") #'elosort-list-top-from-fight)
    map)
  "Fight mode keymap.")

(defun elosort-fight-win1 ()
  "Set p1 as winner and update"
  (interactive)
  (elosort-fight-update t))

(defun elosort-fight-win2 ()
  "Set p2 as winner and update"
  (interactive)
  (elosort-fight-update nil))

(defun elosort-fight-update (p1-winner-p)
  "Update ratings DB for current pair."
  (let* ((p1 (copy-alist elosort-p1))
         (p2 (copy-alist elosort-p2))
         (p1-title (let-alist p1 .title))
         (p2-title (let-alist p2 .title))
         (p1-fights (let-alist p1 .fights))
         (p2-fights (let-alist p2 .fights))
         (p1-vs-p2-num
          (1+ (string-to-number
           (or
            (plist-get p2-fights p1-title #'equal)
            "0"))))
         (p2-vs-p1-num
          (1+ (string-to-number
           (or
            (plist-get p1-fights p2-title #'equal)
            "0"))))
         (p1-elo (or (let-alist p1 .elo) elosort-starting-elo))
         (p2-elo (or (let-alist p2 .elo) elosort-starting-elo))
         (new-elos
          (if
              p1-winner-p
              (elosort-compute-elo p1-elo p2-elo)
            (elosort-compute-elo p2-elo p1-elo)))
         (p1-new-elo (if p1-winner-p (car new-elos) (cdr new-elos)))
         (p2-new-elo (if p1-winner-p (cdr new-elos) (car new-elos)))
         (p1-new-fights (plist-put p1-fights p2-title
                               (number-to-string p2-vs-p1-num) #'equal))
         (p2-new-fights (plist-put p2-fights p1-title
                               (number-to-string p1-vs-p2-num) #'equal)))
    (save-excursion
      (with-current-buffer (find-file-noselect elosort-file)
        (org-entry-put (elosort-get-by-title p1-title) "ELO" (number-to-string p1-new-elo))
        (org-entry-put (elosort-get-by-title p2-title) "ELO" (number-to-string p2-new-elo))
        (org-entry-delete (elosort-get-by-title p1-title) "ELO_FIGHTS")
        (org-entry-delete (elosort-get-by-title p2-title) "ELO_FIGHTS")
        (apply `(org-entry-put-multivalued-property
                 ,(elosort-get-by-title p1-title)
                 "ELO_FIGHTS"
                 ,@p1-new-fights))
        (apply `(org-entry-put-multivalued-property
                 ,(elosort-get-by-title p2-title)
                 "ELO_FIGHTS"
                 ,@p2-new-fights))
        (save-buffer)))
  (elosort-fight-revert)))

(define-derived-mode elosort-fight-mode special-mode "elosort-fight-mode"
  "Fight!"
  (setq buffer-read-only t)
  (defvar-local elosort-buf nil)
  (defvar-local elosort-p1 nil "Candidate #1")
  (defvar-local elosort-p2 nil "Candidate #2"))

(put 'elosort-fight-mode 'mode-class 'special)

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

(defun elosort-next-pair (items)
  "Return (cons item1 item2) from items list."
  (let* ((entries (copy-sequence items))
         (sorted
          (sort entries
                (lambda (a b)
                  (< (let-alist a .num-fights)
                     (let-alist b .num-fights)))))
         (p1 (car sorted))
         (p1-title (let-alist p1 .title))
         (but-p1 (cdr sorted))
         (p2 (car
              (sort but-p1
                    (lambda (a b)
                      (let* ((afs
                              (string-to-number
                               (or (plist-get (let-alist a .fights) p1-title #'equal) "0")))
                             (bfs
                              (string-to-number
                               (or (plist-get (let-alist b .fights) p1-title #'equal) "0"))))
                        (< afs bfs)))))))
    (cons p1 p2)))

(defun elosort-fight-revert ()
  "Refresh buffer, get next pair."
  (interactive)
  (save-excursion
    (with-current-buffer (find-file-noselect elosort-file)
      (setq elosort-question (elosort-get-question))
      (setq entries (shuffle (org-map-entries #'elosort-get-alist)))))
  (let* ((inhibit-read-only t)
         (pair (elosort-next-pair entries))
         (p1 (car pair))
         (p1-elo (let-alist p1 .elo))
         (p1-title (let-alist p1 .title))
         (p2 (cdr pair))
         (p2-elo (let-alist p2 .elo))
         (p2-title (let-alist p2 .title)))
    (setq-local elosort-p1 p1)
    (setq-local elosort-p2 p2)
    (delete-region (point-min) (point-max))
    (when elosort-question
      (insert (format "%s\n" elosort-question)))
    (insert-button p1-title 'action (lambda (_) (elosort-fight-win1)))
    (insert " vs ")
    (insert-button p2-title 'action (lambda (_) (elosort-fight-win2)))
    (insert " ")))


(defun elosort-fight (filename)
  "Start competition."
  (interactive "fSelect file: ")
  (let ((fight-buf
         (get-buffer-create
           "*Org Elo fight")))
    (pop-to-buffer-same-window fight-buf)
    (with-current-buffer fight-buf
      (setq inhibit-read-only t)
      (elosort-fight-mode)
      (setq-local elosort-buf fight-buf)
      (setq elosort-file filename)
      (elosort-fight-revert))))

(defun elosort-fight-from-top ()
  "Call from fight mode providing filename"
  (interactive)
  (elosort-fight elosort-file))

(provide 'elosort)
;;; elosort.el ends here
