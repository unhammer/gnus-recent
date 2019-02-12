;;; gnus-recent-helm.el --- select recently read Gnus articles with helm -*- lexical-binding: t -*-
;;
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.3.2") (gnus-recent "0.2.0") (helm))
;; Keywords: convenience, mail

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Avoid having to open Gnus and find the right group just to get back to
;;; that e-mail you were reading.

;;; To use, require and bind whatever keys you prefer to the
;;; interactive functions:
;;;
;;; (require 'gnus-recent-helm)
;;; (global-set-key (kbd "<f3>") #'gnus-recent-helm)

;;; If you prefer `use-package', the above settings would be:
;;;
;;; (use-package gnus-recent-helm
;;;   :after gnus
;;;   :bind (("<f3>" . gnus-recent-helm)))

;;; Code:

(require 'gnus-recent)
(require 'helm)

(defvar gnus-recent-display-extra nil
  "Display extra article info.")

(defvar gnus-recent-display-levels '(nil 'To 'Cc)
  "Display levels for extra article info.")

(defvar gnus-recent-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<up>") 'gnus-recent-helm-display-cycle)
    map)
  "Keymap for a `helm' source.")

(defmacro gnus-recent-rot1 (ilst)
  `(let ((x (pop ,ilst)))
     (setq ,ilst (append ,ilst (list x)))
     x))

(defmacro gnus-recent-rot1r (ilst)       ;
  `(let ((x (car (last ,ilst))))
     (setq ,ilst (butlast ,ilst))
     (push x ,ilst)
     x))

(defun gnus-recent-helm-display-cycle ()
  "Select the level of article info to display.
The user selects the article infromation display level. Currently only the
  \"default\", \"To\" and \"Cc\" levels are implemented.
The function will refresh the `helm' buffer to display the new level."
  (interactive)
  (setq gnus-recent-display-extra (pop gnus-recent-display-levels))
  (setq gnus-recent-display-levels
        (append gnus-recent-display-levels (list gnus-recent-display-extra)))
  (helm-refresh))

(defun gnus-recent-helm-display-select ()
  "Select the level of article info to display.
The user selects the article infromation display level. Currently only the
  \"default\", \"To\" and \"Cc\" levels are implemented.
The function will refresh the `helm' buffer to display the new level."
  (interactive)
  (let ((display (read-char "Show level: default (d) | To (t) |  Cc (c): ")))
    (cond
     ((eq display ?t) (setq gnus-recent-display-extra 'To))
     ((eq display ?c) (setq gnus-recent-display-extra 'Cc))
     (t (setq gnus-recent-display-extra nil))))
  (helm-refresh))

(defun gnus-recent-helm-candidate-transformer (candidates source)
  "Transform the `helm' data for the display level selected.
This function acts on the filtered data, to show the selected
display inforrmation. CANDIDATES is the list of filtered data.
SOURCE is the `helm' source. Both argumente are passed by `helm',
when the function is called. Also the `helm' multiline feature is
turned on and off, as needed."
  (pcase gnus-recent-display-extra
    ('To (helm-attrset 'multiline nil source)
         (mapcar #'gnus-recent-helm-candidates-display-to candidates))
    ('Cc (helm-attrset 'multiline nil source)
         (mapcar #'gnus-recent-helm-candidates-display-cc candidates))
    (t (assq-delete-all 'multiline source)
       candidates)))

(defun gnus-recent-helm-candidates-display-default (item)
  "The default text to display for each article.
Is the function argument to `mapcar' for establishing the
candidates default text. `Helm' uses this text to filter the
articles list.
For each article data ITEM in `gnus-recent--articles-list' it
returns a cons cell (name . item), where name is the article
display text."
  (cons (concat (car item)
                " ["
                (propertize (alist-get 'group item) 'face '(:foreground "lightblue"))
                "]")
        item))

(defun gnus-recent-helm-candidates-display-to (item)
  "Display the To field for each article on a separate line.
ITEM is the article data in `gnus-recent--articles-list'. Used as the function
  argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipient item)))
        item))

(defun gnus-recent-helm-candidates-display-cc (item)
  "Display the To and Cc fields for each article on separate lines.
ITEM is the article data in `gnus-recent--articles-list'. Used as the function
argument to `mapcar.'"
  (cons (concat (car item)
                "\n    To: "
                (alist-get 'To  (alist-get 'recipient item))
                (helm-aif (alist-get 'Cc  (alist-get 'recipient item))
                    (concat "\n    Cc: " it)))
        item))

(defun gnus-recent-candidates ()
  "Initialize the `helm' candidates data."
  (mapcar #'gnus-recent-helm-candidates-display-default gnus-recent--articles-list))

(defun gnus-recent-helm-forget (_recent)
  "Remove Gnus articles from `gnus-recent--articles-list' using `helm'.
Helm allows for marked articles or current selection.  See
function `helm-marked-candidates'.  Argument _recent is not used."
  (let* ((cand (helm-marked-candidates))
         (l1-cand (length cand))
         (l1-gral (length gnus-recent--articles-list)))
    (dolist (article cand)
      (gnus-recent-forget article t))
    (gnus-message 4
                  "Removed %d of %d article(s) from gnus-recent"
                  (- l1-gral (length gnus-recent--articles-list))
                  l1-cand)))

(defun gnus-recent-helm-forget-pa (recent)
  "Forget current or marked articles without quiting `helm'.
This is the persistent action defined for the helm session.
Argument RECENT is the article data."
  (gnus-recent-helm-forget recent)
  (helm-delete-current-selection)
  (helm-refresh))

(defun gnus-recent-helm ()
  "Use `helm' to filter throu the recently viewed Gnus articles.
Also a number of possible actions are defined."
  (interactive)
  (helm :sources (helm-build-sync-source "Gnus recent articles"
                   :keymap gnus-recent-helm-map
                   :candidates 'gnus-recent-candidates
                   :filtered-candidate-transformer  'gnus-recent-helm-candidate-transformer
                   :persistent-action 'gnus-recent-helm-forget-pa
                   :persistent-help "Forget current selection"
                   :action '(("Open article"               . gnus-recent--open-article)
                             ("Copy org link to kill ring" . gnus-recent-kill-new-org-link)
                             ("Insert org link"            . gnus-recent-insert-org-link)
                             ("Remove marked article(s)"   . gnus-recent-helm-forget)
                             ("Clear all"                  . gnus-recent-forget-all)))
        :buffer "*helm gnus recent*"
        :truncate-lines t))



(provide 'gnus-recent-helm)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; gnus-recent-helm.el ends here
