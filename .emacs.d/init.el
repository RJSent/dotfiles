;; Evaluate a buffer with M-x eval-buffer. Using this in init.el buffer will make Emacs apply any changes without restarting
;; Learn about the major mode and any minor modes that are active for the current buffer with C-h m, describe-mode
;; Learn about a function with C-h f, describe-function
;; Learn about a variable with C-h v, describe-variable
;; Learn about a key binding with C-h k, describe-key. Useful if I remember a binding but not what it does

;;; Packages
;; Autoinstall use-package if we don't have it installed
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("MELPA"        . "https://melpa.org/packages"))
(add-to-list 'package-archive-priorities '("MELPA Stable" . 10))
(add-to-list 'package-archive-priorities '("MELPA"        . 5))
(add-to-list 'package-archive-priorities '("gnu"          . 0))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(use-package rbenv
  :ensure t
  :init (global-rbenv-mode)) ;; Needs symlink to /usr/bin/ruby from $RBENV_ROOT/bin/rbenv if using AUR package
(use-package nord-theme
  :ensure t
  :config (load-theme 'nord t))

;;; Mode definitions
(define-derived-mode mycfg-elisp-mode emacs-lisp-mode "MyConfig Elisp Mode"
  "A mode for my Elisp configs so Flycheck doesn't yell at me.")

;; Autoload modes for the correct files
(add-to-list 'auto-mode-alist
	     '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
	     '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("init.el" . mycfg-elisp-mode))

;;; Splitting init.el across multiple files
(write-region "" "" "custom.el") ;; FIXME: if launched from shell writes to PWD
(setq custom-file (expand-file-name "custom.el" (file-name-directory (or load-file-name buffer-file-name))))
(load custom-file)

;;; Unosrted code goes here
;; Removes empty space at bottom of screen with maximized emacs.
;; emacs.stackexchange.com/questions/34675
(setq frame-resize-pixelwise t)
;; Increases font in default buffer, which all other buffers base off of. May be overwritten by buffer specific fonts
;; stackoverflow.com/questions/294664
(set-face-attribute 'default nil :height 110)
;; Consider fixing commenting with https://stackoverflow.com/questions/26312317
;; Display line numbers on all files. Currently includes shell, want to disable that
;; https://emacs.stackexchange.com/questions/36747 likely has the solution
;; http://ergoemacs.org/emacs/emacs_line_number_mode.html
(global-display-line-numbers-mode)
;; Disable menu bar? Not sure, didn't look for this online but in Emacs
;; Yay, entirely using emacs self help system
(menu-bar-mode -1)
;; Disable tool bar.
(tool-bar-mode -1)
;; Make the initial frame maximized and fullscreen. If you exit fullscreen the frame is still maximized
(toggle-frame-maximized)
(toggle-frame-fullscreen)
;; Disable scroll bar
(scroll-bar-mode -1)
;; Disable hidden text in info files
(setq Info-hide-note-references 1)
;; Inhibit startup message
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(provide 'init)
;;; init.el ends here

