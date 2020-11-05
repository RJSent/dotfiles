;; Evaluate a buffer with M-x eval-buffer. Using this in init.el buffer will make Emacs apply any changes without restarting
;; Learn about the major mode and any minor modes that are active for the current buffer with C-h m, describe-mode
;; Learn about a function with C-h f, describe-function
;; Learn about a variable with C-h v, describe-variable
;; Learn about a key binding with C-h k, describe-key. Useful if I remember a binding but not what it does

;;; Packages
;; Autoinstall use-package if we don't have it installed
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("MELPA"        . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("MELPA Stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archive-priorities '("MELPA"        . 10))
(add-to-list 'package-archive-priorities '("MELPA Stable" . 5))
(add-to-list 'package-archive-priorities '("gnu"          . 0))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package ace-window
  :ensure t
  :bind ("M-o" . 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package which-key
  :ensure t
  :defer 0.2
  :diminish
  :config (which-key-mode))
(use-package all-the-icons
  :ensure t
  :defer 0.5)
(use-package nord-theme ; I prefer Nord but with the hc-zenburn modeline
  :ensure t
  :config (load-theme 'nord t))
;; (use-package hc-zenburn-theme
;;   :ensure t
;;   :config (load-theme 'hc-zenburn t))
(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

(use-package company
  :ensure t
  :diminish
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-frontends '(company-pseudo-tooltip-frontend
			    company-echo-metadata-frontend))
  (setq company-idle-delay 1))
(use-package smartparens
  :ensure t
  :diminish
  :hook (prog-mode . smartparens-mode) ; FIXME not working for enh-ruby-mode although that is part of prog-mode
  :config (require 'smartparens-config))
(use-package highlight-parentheses
  :ensure t
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))
(use-package flycheck
  :ensure t
  :diminish
  :config (global-flycheck-mode))
(use-package diminish
  :ensure t)

(use-package ivy
  :demand
  :ensure t
  :diminish
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (ivy-mode 1))
(use-package ivy-rich
  :ensure t
  :after ivy
  ;; :custom
  ;; (ivy-virtual-abbreviate 'full
  ;;                         ivy-rich-switch-buffer-align-virtual-buffer t
  ;;                         ivy-rich-path-style 'abbrev)
  :config (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
  ;; (ivy-set-display-transformer 'ivy-switch-buffer
  ;;                              'ivy-rich-switch-buffer-transformer))
(use-package counsel
  :ensure t
  :diminish
  :after ivy
  :config (counsel-mode))
(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))
;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :after (all-the-icons ivy)
;;   :config (all-the-icons-ivy-setup))
(use-package all-the-icons-ivy-rich
  :ensure t
  :after (all-the-icons ivy-rich)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package rbenv
  :ensure t
  :diminish
  :hook (enh-ruby-mode . global-rbenv-mode))
(use-package enh-ruby-mode
  :ensure t
  :mode "\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\|Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"
  :interpreter "ruby")
(use-package inf-ruby ;; Latest version has --nomultline by default, but not for bundle console, only ruby
  :ensure t
  :hook (enh-ruby-mode . inf-ruby-minor-mode))
(use-package robe
  :ensure t
  :diminish
  :hook (enh-ruby-mode . robe-mode)
  :config (push 'company-robe company-backends))


;; C-c C-c to execute source blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (C . t)))


;;; Mode definitions that don't fit in with use-package yet
(define-derived-mode mycfg-elisp-mode emacs-lisp-mode "MyConfig Elisp Mode"
  "A mode for my Elisp configs so Flycheck doesn't yell at me... until I get around to making this pretty....")
(add-to-list 'auto-mode-alist '("init.el" . mycfg-elisp-mode))
(diminish 'mycfg-elisp-mode)

;;; Splitting init.el across multiple files
(write-region "" "" (expand-file-name "custom.el" (file-name-directory (or load-file-name buffer-file-name))) t)
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
(toggle-frame-fullscreen)
(toggle-frame-maximized)
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

;; SORT ME
(delete-selection-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init)
;;; init.el ends here

