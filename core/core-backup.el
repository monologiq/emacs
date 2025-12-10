(setq backup-directory-alist `(("." ,core-directory-backup)))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 1
      version-control t)

(provide 'core-backup)
