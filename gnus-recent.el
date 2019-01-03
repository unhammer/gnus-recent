;;; gnus-recent.el --- article breadcrumbs for Gnus -*- lexical-binding: t -*-

;; Copyright (C) 2018 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.2.0
;; URL: https://github.com/unhammer/gnus-recent
;; Package-Requires: ((emacs "25.3.2"))
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
;;; (require 'gnus-recent)
;;; (define-key gnus-summary-mode-map (kbd "l") #'gnus-recent-goto-previous)
;;; (define-key gnus-group-mode-map (kbd "C-c L") #'gnus-recent-goto-previous)

;;; If you prefer `use-package', the above settings would be:
;;;
;;; (use-package gnus-recent
;;;   :after gnus
;;;   :config
;;;   (define-key gnus-summary-mode-map (kbd "l") #'gnus-recent-goto-previous)
;;;   (define-key gnus-group-mode-map (kbd "C-c L") #'gnus-recent-goto-previous))

;;; Code:

(require 'gnus-sum)
(require 'dash)

(defvar gnus-recent--articles-list nil
  "The list of articles read in this Emacs session.")

(defvar gnus-recent--showing-recent nil
  ;; TODO: isn't there some way of showing the calling function?
  "Internal variable; true iff we're currently showing a recent article.")

(defgroup gnus-recent nil
  "Options for gnus-recent"
  :tag "gnus-recent"
  :group 'gnus)

(defface gnus-recent-date-face
  '((t . (:inherit font-lock-type-face)))
  "Face used for dates in the recent article list."
  :group 'gnus-recent)

(defun gnus-recent--get-article-data ()
  "Get the article data used for gnus-recent."
  (unless gnus-recent--showing-recent
    (set-buffer gnus-summary-buffer)
    (let ((article-number
           ;; based on gnus-summary-article-header (a macro which fails here):
           (gnus-data-header (gnus-data-find
                              (progn
                                (gnus-summary-skip-intangible)
                                (or (get-text-property (point) 'gnus-number)
                                    (gnus-summary-last-subject)))))))
      (list
       (format "%s: %s \t%s"
               (propertize
                (replace-regexp-in-string "\\([^\<]*\\) <\\(.*\\)>" "\\1"
                                          (replace-regexp-in-string "\"\\([^\"]*\\)\" <\\(.*\\)>" "\\1"
                                                                    (mail-header-from article-number)))
                'face 'bold)
               (mail-header-subject article-number)
               (propertize (mail-header-date article-number) 'face 'gnus-recent-date-face))
       (mail-header-id article-number)
       gnus-newsgroup-name))))

(defun gnus-recent--track-article ()
  "Store this article in the recent article list.
For tracking of Backend moves (B-m) see `gnus-recent--track-move-article'."
  (let ((article-data (gnus-recent--get-article-data)))
    (when article-data
      (add-to-list 'gnus-recent--articles-list article-data)))
  (setq gnus-recent--showing-recent nil))

(add-hook 'gnus-article-prepare-hook 'gnus-recent--track-article)

(defun gnus-recent--track-move-article (action _article _from-group to-group _select-method)
  "Track backend move (B-m) of articles.
When ACTION is 'move, will change the group to TO-GROUP for the
article data in `gnus-recent--articles-list', but only if the
moved article was already tracked.  For use by
`gnus-summary-article-move-hook'."
  (when (eq action 'move)
    (let ((article-data (gnus-recent--get-article-data)))
      (cl-nsubstitute (list (first article-data) (second article-data) to-group) 
                      article-data
                      gnus-recent--articles-list
                      :test 'equal :count 1))))

(add-hook 'gnus-summary-article-move-hook 'gnus-recent--track-move-article)

(defun gnus-recent--track-delete-article (action ghead group &rest rest)
  "Track interactive user deletion of articles and remove the
article data in `gnus-recent--articles-list'. This function is
applied by the abnormal hook `gnus-summary-article-delete-hook'."
  (when (eq action 'delete)
    (cl-delete (gnus-recent--get-article-data)
               gnus-recent--articles-list
               :test 'equal :count 1)))

(add-hook 'gnus-summary-article-delete-hook 'gnus-recent--track-delete-article)

(defmacro gnus-recent--shift (lst)
  "Put the first element of LST last, then return that element."
  `(let ((top (pop ,lst)))
     (setq ,lst (nconc ,lst (list top)) )
     top))

(defun gnus-recent-goto-previous (&optional no-retry)
  "Go to the top of the recent article list.
Unless NO-RETRY, we try going further back if the top of the
article list is the article we're currently looking at."
  (interactive)
  (let ((gnus-recent--showing-recent t))
    (if (not gnus-recent--articles-list)
        (message "No recent article to show")
      (gnus-recent--action
       (gnus-recent--shift gnus-recent--articles-list)
       (lambda (message-id group)
         (if (and (not no-retry)
                  (equal (current-buffer) gnus-summary-buffer)
                  (equal message-id (mail-header-id (gnus-summary-article-header))))
             (gnus-recent-goto-previous 'no-retry)
           (gnus-summary-read-group group 1) ; have to show at least one old one
           (gnus-summary-refer-article message-id)))))))

(defun gnus-recent--action (recent func)
  "Find message-id and group arguments from RECENT, call FUNC on them.
Warn if RECENT can't be deconstructed as expected."
  (pcase recent
    (`(,_ . (,message-id ,group . ,_))
     (funcall func message-id group))
    (_
     (message "Couldn't parse recent message: %S" recent))))

(defun gnus-recent--open-article (recent)
  "Open RECENT gnus article."
  (gnus-recent--action
   recent
   (lambda (message-id group)
     (let* ((gnus-recent--showing-recent t))
       (if (and (equal (current-buffer) gnus-summary-buffer)
                (equal message-id (mail-header-id (gnus-summary-article-header))))
           (call-interactively 'gnus-recent-goto-previous)
         (gnus-summary-read-group group 1) ; have to show at least one old one
         (gnus-summary-refer-article message-id))))))

(defun gnus-recent--create-org-link (recent)
  "Return an `org-mode' link to RECENT Gnus article."
  (gnus-recent--action
   recent
   (lambda (message-id group)
      (format "[[gnus:%s#%s][Email from %s]]"
                group
                (replace-regexp-in-string "^<\\|>$"
                                          ""
                                          message-id)
                (replace-regexp-in-string "[][]"
                                          ""
                                          (substring (car recent) 0 48))))))

(defun gnus-recent-kill-new-org-link (recent)
  "Add to the `kill-ring' an `org-mode' link to RECENT Gnus article."
  (kill-new (gnus-recent--create-org-link recent))
  (message "Added org-link to kill-ring"))

(defun gnus-recent-insert-org-link (recent)
  "Insert an `org-mode' link to RECENT Gnus article."
  (insert (gnus-recent--create-org-link recent)))

(defun gnus-recent-forget (recent)
  "Remove RECENT Gnus article from `gnus-recent--articles-list'."
  (cl-delete recent gnus-recent--articles-list :test 'equal :count 1)
    (message "Removed %s from gnus-recent articles" (car recent)))


(provide 'gnus-recent)
;;; gnus-recent.el ends here
