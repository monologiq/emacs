;;; core-elisp.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(defun core/insert-comment-header ()
  "Insert comment header with lexical binding, code and commentary blocks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert ";;; ")
    (insert (file-name-base buffer-file-name))
    (insert ".el --- ")
    (insert " -*- lexical-binding: t -*-\n\n")
    (insert ";;; Commentary:\n\n")
    (insert ";;; Code:\n")
    (goto-char (point-max))
    (insert"\n(provide '")
    (insert (file-name-base buffer-file-name))
    (insert ")\n")
    (insert ";;; ")
    (insert (file-name-base buffer-file-name))
    (insert ".el ends here")))

(provide 'core-elisp)
;;; core-elisp.el ends here
