;;; core-env.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(defgroup core-directories nil
  "Customization group for the code related direcoties."
  :tag "Core directories"
  :group 'environment)

(defcustom core-directory-base
  (expand-file-name ".etc" user-emacs-directory)
  "Path to the fallback directory is XDG envvars are not set."
  :type 'directory
  :group 'core-directories)

(defcustom core-directory-cache nil
  "Path to the cache directory."
  :type 'directory
  :group 'core-directories)

(defcustom core-directory-data nil
  "Path to the data directory."
  :type 'directory
  :group 'core-directories)

(defcustom core-directory-state nil
  "Path to the state directory."
  :type 'directory
  :group 'core-directories)

(defcustom core-directory-backup nil
  "Path to the backup files directory."
  :type 'directory
  :group 'core-directories)

(defvar xdg-cache-home (getenv "XDG_CACHE_HOME")
  "Path to XDG cache directory.")

(defvar xdg-data-home (getenv "XDG_DATA_HOME")
  "Path to XDG data directory.")

(defvar xdg-state-home (getenv "XDG_STATE_HOME")
  "Path to XDG state directory.")

(dolist (dir '((core-directory-cache  "cache")
               (core-directory-data   "data")
               (core-directory-config "config")
               (core-directory-backup "backup")))
  (pcase-let ((`(,variable ,dirname) dir))
    (let ((path (expand-file-name dirname core-directory-base)))
      (unless (file-exists-p path)
        (make-directory path t))
      (set variable path))))
		     
(provide 'core-env)
;;; core-env.el ends here
