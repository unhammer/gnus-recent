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

(defun gnus-recent-helm ()
  "Select a recent Gnus article to open with `helm'."
  (interactive)
  (helm :sources `(((name . "Gnus recent articles")
                    (candidates . ,(mapcar (lambda (item)
                                             (cons (car item) item))
                                           gnus-recent--articles-list))
                    (action . (("Open article"               . gnus-recent--open-article)
                               ("Copy org link to kill ring" . gnus-recent-kill-new-org-link)
                               ("Insert org link"            . gnus-recent-insert-org-link)
                               ("Remove article"             . gnus-recent-helm-forget)
                               ("Clear all"                  . gnus-recent-forget-all)))))
        :buffer "*helm gnus recent*"))

(defun gnus-recent-helm-forget (_recent)
  "Remove Gnus articles from `gnus-recent--articles-list' using `helm'.
Helm allows for marked articles or current selection. See
function `helm-marked-candidates'. Argument _recent is not used."
  (let ((cand (helm-marked-candidates)))
    (dolist (article cand)
      (if (equal article (car gnus-recent--articles-list))
          (pop gnus-recent--articles-list)
        (cl-delete article gnus-recent--articles-list :test 'equal :count 1)))
    (message "Removed %d article(s) from gnus-recent" (length cand))))

(provide 'gnus-recent-helm)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; gnus-recent-helm.el ends here
