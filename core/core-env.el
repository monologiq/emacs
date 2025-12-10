;;; core-env.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(defgroup core-directories nil
  "Customization group for the code related direcoties."
  :tag "Core directories"
  :group 'environment)

(defcustom core-directory-fallback nil
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

(setq core-directory-fallback
      (expand-file-name ".etc" user-emacs-directory))

(setq core-directory-cache
      (expand-file-name
       "emacs"
       (if xdg-cache-home
	   xdg-cache-home
	 core-directory-fallback)))

(setq core-directory-data
      (expand-file-name
       "emacs"
       (if xdg-data-home
           xdg-data-home
         core-directory-fallback)))

(setq core-directory-state
      (expand-file-name
       "emacs"
       (if xdg-state-home
           xdg-state-home
         core-directory-fallback)))

(setq core-directory-backup
      (expand-file-name "backups" core-directory-cache))

(dolist (dir '(core-directory-cache
	       core-directory-data
	       core-directory-state
	       core-directory-backup))
  (unless (file-exists-p (symbol-value dir))
    (make-directory (symbol-value dir) t)))

(provide 'core-env)

;;; core-env.el ends here
