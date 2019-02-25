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
(require 'org-gnus)
(require 'rfc2047)
(require 'helm-lib)

(defvar gnus-recent--articles-list nil
  "The list of articles kept by gnus-recent.")

(defvar gnus-recent--showing-recent nil
  ;; TODO: isn't there some way of showing the calling function?
  "Internal variable; true iff we're currently showing a recent article.")

(defgroup gnus-recent nil
  "Article breadcrumbs for gnus."
  :tag "Gnus Recent"
  :group 'gnus)

(defcustom gnus-recent-file  "~/.gnus-recent-data"
  "The file to save the gnus-recent-articles list data."
  :group 'gnus-recent
  :type 'file)

(defcustom gnus-recent-format-time-string "%F %T %a"
  "A string for formating the article date.
The format is used by `format-time-string'. See its documentation
for details on format specifiers. For example, to produce a full
ISO 8601 format, use \"%FT%T%z\", for org style use \"%F %T %a\".
Changing this variable affects only new entries. Previous entries
keep the old format."
  :group 'gnus-recent
  :type 'string)

(defface gnus-recent-group-face
  '((t . (:inherit font-lock-type-face :foreground "lightblue")))
  "Face used for gnus group in the recent articles list."
  :group 'gnus-recent)

(defface gnus-recent-date-face
  '((t . (:inherit font-lock-type-face)))
  "Face used for dates in the recent articles list."
  :group 'gnus-recent)

(defun gnus-recent-date-format (date)
  "Convert the DATE to string.
Date format specified in `gnus-recent-format-time-string'."
  (condition-case ()
      (format-time-string gnus-recent-format-time-string (gnus-date-get-time date))
    (error "Error in date format conversion")))

(defun gnus-recent-get-email (address &optional unbracket)
  "Get the email portion of a gnus address.
ADDRESS is a gnus sender or recipient address string. When optional
argument UNBRACKET is non-nil, brackets will be trimmed from the
email."
  (car (last (split-string address " " t (and unbracket "<\\|>")))))

(defun gnus-recent-get-email-name (address &optional use-email decode)
  "Get the name portion of a gnus address.
ADDRESS is a gnus sender or recipient address string. If a name
is not found and USE-EMAIL is not nil, use the email address as
default. If DECODE, RFC2047 decoding will be applied to the
display-name."
  (let ((x (split-string-and-unquote address)))
    (if (eql 1 (length x))
        (or (and use-email (car x)) "")
      (let ((name (string-join (butlast x) " ")))
        (if (and decode
                 (string= "=?" (substring name 0 2)))
            (org-strip-quotes (rfc2047-decode-address-string name))
          name)))))

(defun gnus-recent--get-article-data ()
    "Get the article data used for `gnus-recent' based on `gnus-summary-article-header'."
    (unless gnus-recent--showing-recent
      (let* (;; not needed
             ;; (article-number (gnus-summary-article-number))
             ;; (article-header (gnus-summary-article-header article-number))
             (article-header (gnus-summary-article-header))
             (date  (gnus-recent-date-format (mail-header-date article-header)))
             (subject (mail-header-subject article-header))
             (author (mail-header-from article-header))
             (recipients (mail-header-extra article-header)))
        (dolist (r recipients)
          (setcdr r (rfc2047-decode-address-string (cdr r))))
        (list (format "%s: %s \t%s"
                      (propertize (gnus-recent-get-email-name author t) 'face 'bold)
                      subject
                      (propertize date 'face 'gnus-recent-date-face))
              (cons 'group gnus-newsgroup-name)
              (cons 'message-id (mail-header-id article-header))
              (cons 'date date)
              (cons 'subject subject)
              (cons 'sender author)
              (cons 'recipients recipients)))))

(defun gnus-recent--track-article ()
  "Store this article in the recent article list.
For tracking of Backend moves (B-m) see `gnus-recent--track-move-article'."
  (gnus-recent-add-to-list (gnus-recent--get-article-data))
  (setq gnus-recent--showing-recent nil))

(defun gnus-recent--track-move-article (action article _from-group to-group _select-method)
  "Track backend move (B-m) of articles.
When ACTION is 'move, will change the group to TO-GROUP for the
article data in `gnus-recent--articles-list', but only if the
moved article was already tracked. ARTICLE is the gnus message
header. For use by `gnus-summary-article-move-hook', so all
arguments are passed by gnus."
  (when (eq action 'move)
    (if to-group
        (gnus-recent-update-message-id (mail-header-id article) to-group)
      (message-box "Move article to EmptyGroup! Probable Error!\n"))))

(defun gnus-recent--track-delete-article (action article _from-group &rest _rest)
  "Track interactive user deletion of articles.
Remove the article data in `gnus-recent--articles-list'. ACTION
should be 'delete. ARTICLE is the gnus message header. For use by
`gnus-summary-article-delet-hook', so all arguments are passed by
gnus."
  (when (eq action 'delete)
    (gnus-recent-forget-message-id (mail-header-id article))))

(defun gnus-recent--track-expire-article (action article _from-group to-group _select-method)
  "Track when articles expire.
Handle the article data in `gnus-recent--articles-list',
according to the expiry ACTION. TO-GROUP should have the value of
the expiry-target group if set. ARTICLE is the gnus message
header passed when the hook is run. For use by
`gnus-summary-article-expire-hook'."
  (when (eq action 'delete)
    (if to-group                        ; article moves to the expiry-target group
        (gnus-recent-update-message-id (mail-header-id article) to-group)
      (gnus-recent-forget-message-id (mail-header-id article))))) ; article deleted

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
  "Find `message-id' and group arguments from RECENT, call FUNC on them.
Warn if RECENT can't be deconstructed as expected."
  (pcase recent
    (`(,_ . (,message-id ,group . ,_))
     (funcall func message-id group))
    (_
     (message "Couldn't parse recent message: %S" recent))))

(defun gnus-recent--open-article (recent)
  "Open RECENT gnus article using `org-gnus'."
  (org-gnus-follow-link (alist-get 'group recent) (alist-get 'message-id recent)))

(defun gnus-recent--create-org-link (recent)
  "Return an `org-mode' link to RECENT Gnus article."
  (format "[[gnus:%s#%s][Email from %s]]"
          (alist-get 'group recent)
          (replace-regexp-in-string "^<\\|>$"
                                    ""
                                    (alist-get 'message-id recent))
          (replace-regexp-in-string "[][]"
                                    ""
                                    (substring (car recent) 0 48))))

(defun gnus-recent-kill-new-org-link (recent)
  "Add to the `kill-ring' an `org-mode' link to RECENT Gnus article."
  (kill-new (gnus-recent--create-org-link recent))
  (message "Added org-link to kill-ring"))

(defun gnus-recent-insert-org-link (recent)
  "Insert an `org-mode' link to RECENT Gnus article."
  (insert (gnus-recent--create-org-link recent)))

(defun gnus-recent-update-message-id (message-id to-group)
  "Update the Gnus article with MESSAGE-ID in `gnus-recent--articles-list'.
The Gnus article has moved to group TO-GROUP."
  (let ((article (gnus-recent-find-message-id message-id)))
    (when article
      (setf (alist-get 'group article) to-group))))

(defun gnus-recent-update (recent to-group)
  "Update RECENT Gnus article in `gnus-recent--articles-list'.
The Gnus article has moved to group TO-GROUP."
  (gnus-recent-update-message-id (alist-get 'message-id recent) to-group))

(defun gnus-recent-forget-message-id (message-id &optional print-msg)
  "Remove the Gnus article with MESSAGE-ID in `gnus-recent--articles-list'.
When PRINT-MSG is non-nil, show a message about it."
  (let ((l1 (length gnus-recent--articles-list))
        (article (car gnus-recent--articles-list)))
    ;; check for a match on the first article on list
    (if (equal message-id (alist-get 'message-id article))
        (pop gnus-recent--articles-list)
      (setq article (gnus-recent-find-message-id message-id))
      (cl-delete article gnus-recent--articles-list :test 'equal :count 1))
    (when print-msg
      (gnus-message 4 "Removed %d of 1 from gnus-recent articles" (- l1 (length gnus-recent--articles-list)))
      (gnus-message 4 "Removed item: %s from gnus-recent articles" (car article)))))

(defun gnus-recent-forget (recent &optional print-msg)
  "Remove RECENT Gnus article from `gnus-recent--articles-list'.
When PRINT-MSG is non-nil, show a message about it."
  (gnus-recent-forget-message-id (alist-get 'message-id recent) print-msg))

(defun gnus-recent-forget-all (&rest _recent)
  "Clear the gnus-recent articles list."
  (interactive)
  (setq gnus-recent--articles-list nil)
  (gnus-message 4 "Cleared all gnus-recent article entries"))

(defun gnus-recent-bbdb-display-all (recent)
  "Display sender and recipients in BBDB.
Diplay sender and all recipients in BBDB. Ask to create a BBDB entry, if not in
BBDB. RECENT is the gnus-recent data for the selected article."
  (let ((recipients (alist-get 'recipients recent))
        (search-list '(bbdb-search (bbdb-records))))
    (setq recipients (append (bbdb-split "," (or (alist-get 'sender recent) ""))
                             (bbdb-split "," (or (alist-get 'To recipients) ""))
                             (bbdb-split "," (or (alist-get 'Cc recipients) ""))))
    (dolist (r recipients)              ; add new entries to BBDB (ask)
      (bbdb-update-records (list (list (gnus-recent-get-email-name r t)
                                       (gnus-recent-get-email r t)))
                           'query t))
    ;; (bbdb-update-records                ; add new entries to BBDB (ask)
    ;;  (mapcar (lambda (r)                ; this also works, but sometimes skips remainders
    ;;            (list (gnus-recent-get-email-name r t)
    ;;                  (gnus-recent-get-email r t)))
    ;;          recipients)
    ;;  'query t)

    ;; make an array (:mail email1 :mail email2 ...etc)
    (dolist (r recipients search-list)
      (helm-aif (gnus-recent-get-email r t)
          (nconc search-list (list :mail it))))

    ;; combine:
    ;; (bbdb-display records (bbdb-search (bbdb-records :mail email1 :mail
    ;; email2...etc)

    (setq search-list (eval search-list))
    (if search-list
        (bbdb-display-records search-list 'multi-line nil)
      (gnus-message 4 "No matching BBDB records found"))))

;; FIXME: this function text is only a placeholder.
;; Now that gnus-recent uses the message-id to handle the articles in its'
;; article list, there should be no duplicates entries. Still, this function will
;; help with checking consistency, its just not that critical at the moment.
(defun gnus-recent-alist-find-duplicates ()
  "Find any duplicate entries in `gnus-recent--articles-list'.
Duplicates entries are considered those that have the same
message-id, even if some other property may differ such as the
group value. It returns a list of message-ids that are found more
than once."
 (cl-find elem1 gnus-recent--articles-list))

(defun gnus-recent-filter-prop (prop value)
  "Return a list  of all articles with PROP equal to VALUE.
Search the `gnus-recent--articles-list' for all elements with
property PROP equal to value."
  (seq-filter #'(lambda (item)
                  (equal value (alist-get prop item)))
              gnus-recent--articles-list))

(defun gnus-recent-find-prop (prop value)
  "Check for an article with the property value given.
Find in `gnus-recent--articles-list' if there is a property PROP equal to VALUE.
Returns the first article data when a match is found. It does not try
to find any more matches."
  (seq-find #'(lambda (item)
            (equal value (alist-get prop item)))
        gnus-recent--articles-list))

(defun gnus-recent-find-message-id (message-id)
  "Search the gnus-recent articles data by MESSAGE-ID.
Returns the first article in `gnus-recent--articles-list' that
matches the MESSAGE-ID provided. A convinience wrapper for
`gnus-recent-find-prop'."
  (gnus-recent-find-prop 'message-id  message-id))

(defun gnus-recent-find-article (recent)
  "Search the gnus-recent articles list for RECENT article.
Returns the first article in `gnus-recent--articles-list' that
matches the message-id of the RECENT article argument."
  (gnus-recent-find-message-id (alist-get 'message-id recent)))

(defun gnus-recent-add-to-list (recent)
  "Add the RECENT article data to the articles list.
Ensures the value for messsage-id is unique among all articles
stored in `gnus-recent--articles-list'. See
`gnus-recent--get-article-data' for the recent article data
format."
  (when recent
    (unless (gnus-recent-find-message-id (alist-get 'message-id recent))
      (push recent gnus-recent--articles-list))))

;; TODO: can we save the diff, instead of everything ?
(defun gnus-recent-save ()
  "Save the gnus recent items to file for persistance."
  (interactive)
  (if (file-writable-p gnus-recent-file)
      (progn
        (gnus-message 5 "Saving gnus-recent data (%d items) to %s."
                      (length gnus-recent--articles-list) gnus-recent-file)
        (with-temp-file gnus-recent-file
          (print gnus-recent--articles-list (current-buffer)))
        (gnus-message 5 "Saving gnus-recent data done."))
    (error "Error: can not save gnus-recent data to %s" gnus-recent-file)))

(defun gnus-recent-read ()
  "Read gnus-recent data from a previous session."
  (interactive)
  (gnus-message 5 "Reading gnus-recent data from %s." gnus-recent-file)
  (setq gnus-recent--articles-list
        (if (file-readable-p gnus-recent-file)
            (read
             (with-temp-buffer
               (insert-file-contents gnus-recent-file)
               (buffer-string)))
          nil))
  (gnus-message 5 "Read %d item(s) from %s... done."
                (length gnus-recent--articles-list) gnus-recent-file))

(defun gnus-recent-start ()
  "Start Gnus Recent."
  (interactive)
  (gnus-message 5 "Starting gnus-recent")
  (gnus-recent-add-hooks)
  (gnus-recent-read))

(defun gnus-recent-add-hooks ()
  "Install the gnus-recent hooks."
  (interactive)
  ;; Activate the hooks  (should be named -functions)
  ;; Note: except for the 1st, the other hooks run using run-hook-with-args
  (add-hook 'gnus-article-prepare-hook        'gnus-recent--track-article)
  (add-hook 'gnus-summary-article-move-hook   'gnus-recent--track-move-article)
  (add-hook 'gnus-summary-article-delete-hook 'gnus-recent--track-delete-article)
  (add-hook 'gnus-summary-article-expire-hook 'gnus-recent--track-expire-article)
  ;; hooks related to saving the data
  (add-hook 'gnus-save-newsrc-hook 'gnus-recent-save)
  (add-hook 'kill-emacs-hook 'gnus-recent-save))

(defun gnus-recent-remove-hooks ()
  "Remove the gnus-recent hooks."
  (interactive)
  (remove-hook 'gnus-article-prepare-hook        'gnus-recent--track-article)
  (remove-hook 'gnus-summary-article-move-hook   'gnus-recent--track-move-article)
  (remove-hook 'gnus-summary-article-delete-hook 'gnus-recent--track-delete-article)
  (remove-hook 'gnus-summary-article-expire-hook 'gnus-recent--track-expire-article)
  ;; hooks related to saving the data
  (remove-hook 'gnus-save-newsrc-hook 'gnus-recent-save)
  (remove-hook 'kill-emacs-hook 'gnus-recent-save))

;; start gnus-recent session
(gnus-recent-start)

(provide 'gnus-recent)
;;; gnus-recent.el ends here
