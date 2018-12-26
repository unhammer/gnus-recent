;;; gnus-recent-helm.el --- select recently read Gnus articles with helm -*- lexical-binding: t -*-

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
(require 'helm-sources)

(defun gnus-recent-helm ()
  "Select a recent Gnus article to open with `helm'."
  (interactive)
  (helm :sources `(((name . "Gnus recent articles")
                    (candidates . ,(mapcar (lambda (item)
                                             (cons (car item) item))
                                           gnus-recent--articles-list)) 
                    (action . (("Insert Org link" . gnus-recent-insert-org-link)
                               ("Open article"    . gnus-recent--open-article)
                               ("Forget article"  . gnus-recent-forget)))))
        :buffer "*helm gnus recent*"))

(provide 'gnus-recent-helm)
;;; gnus-recent-helm.el ends here
