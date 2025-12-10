(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))

(require 'core-env)

(require 'core-backup)

(require 'init-no-littering)
