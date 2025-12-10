;;; core-package-no-littering.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq no-littering-etc-directory core-directory-config
      no-littering-var-directory core-directory-data)

(require 'no-littering nil t)

(provide 'core-package-no-littering)
;;; core-package-no-littering.el ends here
