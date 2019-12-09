;;; gnus-recent-ivy.el --- select recently read Gnus articles with ivy -*- lexical-binding: t -*-

;; Copyright (C) 2018 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.2.0
;; URL: https://github.com/unhammer/gnus-recent
;; Package-Requires: ((emacs "25.3.2") (gnus-recent "0.2.0") (ivy "0.9.0"))
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
;;; (require 'gnus-recent-ivy)
;;; (global-set-key (kbd "<f3>") #'gnus-recent-ivy)

;;; If you prefer `use-package', the above settings would be:
;;;
;;; (use-package gnus-recent-ivy
;;;   :after gnus
;;;   :bind (("<f3>" . gnus-recent-ivy)))

;;; Code:

(require 'gnus-recent)
(require 'ivy)

(defun gnus-recent-ivy ()
  "Select a recent Gnus article to open with `ivy'."
  (interactive)
  (ivy-read "Recent article: "
            gnus-recent--articles-list
            :action #'gnus-recent--open-article
            :require-match t
            :re-builder #'ivy--regex-plus))

(ivy-add-actions #'gnus-recent-ivy
                 '(("l" gnus-recent-insert-org-link "insert org link")
                   ("c" gnus-recent-kill-new-org-link "copy org link")
                   ("k" gnus-recent-forget "forget")
                   ("K" gnus-recent-forget-all "forget all")))


(provide 'gnus-recent-ivy)
;;; gnus-recent-ivy.el ends here
