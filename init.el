;;; init.el
(require 'core-env)

(require 'core-backup)

(tool-bar-mode -1)
(menu-bar-mode -1)

(require 'meow)
(require 'meow-tree-sitter)

(with-eval-after-load 'meow-tree-sitter
  (meow-tree-sitter-register-defaults))

(setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-iso)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . comment-or-uncomment-region)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)		
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(with-eval-after-load 'meow
  (meow-setup)
  (meow-global-mode 1))

;; (require 'auto-dark)
;; (setq auto-dark-themes '((modus-vivendi) (modus-operandi)))

(load-theme 'modus-vivendi)

;; Set SF Pro as the default font for Emacs GUI
(set-face-attribute 'default nil
                    :family "SF Pro"
                    :height 130)

;; Set Iosevka Cavalier for programming modes
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka Cavalier"
                    :height 120)

;; Apply fixed-pitch (Iosevka Cavalier) to prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (buffer-face-mode)
            (buffer-face-set '(:family "Iosevka Cavalier" :height 120))))

;; For org-mode: use SF Pro for text, Iosevka Cavalier for code blocks
(add-hook 'org-mode-hook
          (lambda ()
            ;; Enable variable-pitch-mode for proportional fonts in org text
            (variable-pitch-mode 1)
            ;; Set faces for code blocks to use fixed-pitch (Iosevka Cavalier)
            (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))

(add-hook 'vterm-mode-hook
          (lambda ()
	    (buffer-face-mode)
            (buffer-face-set '(:family "Iosevka Cavalier" :height 120))))



;; Optional: Set variable-pitch to SF Pro explicitly
(set-face-attribute 'variable-pitch nil
                    :family "SF Pro"
                    :height 130)

;; (with-eval-after-load 'auto-dark
;;   (auto-dark-mode))

(require 'magit)

;; (require 'pdf-tools)

(setq tab-always-indent 'complete
      text-mode-ispell-word-completion nil
      read-extended-command-predicate #'command-completion-default-include-p)
(require 'corfu)

(with-eval-after-load 'corfu
  (global-corfu-mode));; (elpaca transient)
;; (elpaca (magit :wait t))

(setq flymake-fringe-indicator-position nil
      flymake-suppress-zero-counters t)
(require 'treesit)
(require 'eglot)
(require 'nix-ts-mode)
(with-eval-after-load 'nix-ts-mode
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nixd")))
  (add-hook 'nix-ts-mode-hook 'eglot-ensure))

;; Add custom theme directory
;; (setq custom-theme-directory "~/.config/emacs/themes")
;; (add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

(add-to-list 'custom-theme-load-path 
             (expand-file-name "themes" user-emacs-directory))

;; Load your font theme
(load-theme 'concentration t)

;; Enable variable-pitch-mode for org
(add-hook 'org-mode-hook 'variable-pitch-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("067fb8f548cc27f3d66db3fb35a5e282fc9e945087af32c511278881c9d8b903"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
