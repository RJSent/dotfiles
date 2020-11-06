;;; init.el --- Initialization file for Emacs
;;
;;
;;; Commentary:
;; It's what you'd expect.
;;
;;; Code:


(eval-and-compile			; old.reddit.com/r/emacs/comments/gwupwt/noob_please_help_to_resolve_the_flycheck_error/ft1kk2j
  (defvar straight-fix-flycheck t)      ; github.com/raxod502/straight.el#integration-with-flycheck
  (defvar bootstrap-version)		; Install straight.el
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package))	; Install use-package


;;; Navigation, aesthetic, and other global packages

(use-package ace-window
  :straight t
  :defines aw-keys
  :functions ace-window
  :bind* ("M-o" . 'ace-window) ; * as ibuffer overrides M-o. Consider adjusting as M-o is used for ivy-dispatching done
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package which-key
  :straight t
  :defines which-key-add-column-padding
  :functions which-key-mode
  :defer 1
  :diminish
  :config (which-key-mode)
  (setq which-key-add-column-padding 3))
(use-package all-the-icons
  :straight t)
(use-package diminish
  :straight t)
(use-package nord-theme ; I prefer Nord but with the hc-zenburn modeline
  :straight t
  :config (load-theme 'nord t))
;; (use-package hc-zenburn-theme
;;   :straight t
;;   :config (load-theme 'hc-zenburn t))
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))
(use-package projectile
  :straight t
  :functions projectile-mode
  :diminish
  :config (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package all-the-icons-ibuffer
  :straight t
  :diminish
  :after all-the-icons)
(use-package ibuffer-vc ; Also consider ibuffer-projectile
  :straight t
  :defines ibuffer-sorting-mode ibuffer-inline-columns ibuffer-formats
  :functions ibuffer-vc-set-filter-groups-by-vc-root ibuffer-do-sort-by-alphabetic
  :after all-the-icons-ibuffer
  :hook (ibuffer . (lambda () (ibuffer-vc-set-filter-groups-by-vc-root)
		     (unless (eq ibuffer-sorting-mode 'alphabetic)
		       (ibuffer-do-sort-by-alphabetic))))
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t) ; FIXME: file sizes don't match reality, but it's not just a matter of 1024 vs. 1000.
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))))
  (setq ibuffer-formats
	'((mark modified read-only vc-status-mini " "
		(icon 2 2 :center :elide)
		" "
		(name 18 18 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 20 20 :left :elide)
		" "
		(vc-status 16 16 :left)
		" "
		vc-relative-file))))
(use-package doom-modeline ; Later, replace with custom following similar process to https://www.gonsie.com/blorg/modeline.html
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))
(use-package highlight-parentheses
  :straight t
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))
(use-package wgrep
  :straight t
  :defer 1)


;;; Packages for general purpose programming and editing

(use-package company
  :straight t
  :defines company-minimum-prefix-length company-frontends company-idle-delay
  :diminish
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-frontends '(company-pseudo-tooltip-frontend
			    company-echo-metadata-frontend))
  (setq company-idle-delay 1))
(use-package smartparens
  :straight t
  :diminish
  :hook (prog-mode . smartparens-mode) ; FIXME not working for enh-ruby-mode although that is part of prog-mode
  :config (require 'smartparens-config))
(use-package flycheck
  :straight t
  :functions global-flycheck-mode
  :diminish
  :defer 1
  :config ;(setq flycheck-emacs-lisp-load-path 'inherit) ; FIXME: Currently errors pop up after evaluating init buffer
  (global-flycheck-mode))                               ; (not at first), as if it's not detecting use-package


;;; Packages for ivy and ivy integration

(use-package ivy
  :straight t
  :defines ivy-use-virtual-buffers ivy-count-format ivy-wrap ivy-format-functions-alist ivy-format-functions-alist
  :functions ivy-mode ivy-format-function-line
  :demand
  :diminish
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq ivy-wrap t)
  (ivy-mode 1))
(use-package ivy-rich
  :straight t
  :functions ivy-rich-mode
  :after ivy counsel
  :config (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
(use-package counsel
  :straight t
  :functions counsel-mode
  :diminish
  :after ivy
  :config (counsel-mode))
(use-package swiper
  :straight t
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))
(use-package all-the-icons-ivy-rich
  :straight t
  :after all-the-icons ivy-rich
  :functions all-the-icons-ivy-rich-mode
  :config (all-the-icons-ivy-rich-mode 1))
(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :functions counsel-projectile-mode
  :config (counsel-projectile-mode))


;;; Ruby programming packages

(use-package rbenv
  :straight t
  :diminish
  :hook (enh-ruby-mode . global-rbenv-mode))
(use-package enh-ruby-mode
  :straight t
  :mode "\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\|Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"
  :interpreter "ruby")
(use-package inf-ruby ; Latest version has --nomultline by default, but not for bundle console, only ruby
  :straight t
  :hook (enh-ruby-mode . inf-ruby-minor-mode))
(use-package robe
  :straight t
  :defines company-backends
  :diminish
  :hook (enh-ruby-mode . robe-mode)
  :config (push 'company-robe company-backends))


;;; Some less well organized code.

(org-babel-do-load-languages ; Org-babel language support
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (C . t)))

;; Readded due to FIXME in flycheck. Disables syntax highlighting in init.el
;; (define-derived-mode mycfg-elisp-mode emacs-lisp-mode "MyConfig Elisp Mode"
;;   "A mode for my Elisp configs so Flycheck doesn't yell at me.")
;; (add-to-list 'auto-mode-alist '("init.el" . mycfg-elisp-mode))


;; Support for custom.el
(write-region "" "" (expand-file-name "custom.el" (file-name-directory (or load-file-name buffer-file-name))) t)
(setq custom-file (expand-file-name "custom.el" (file-name-directory (or load-file-name buffer-file-name))))
(load custom-file)

;; Removes empty space at bottom of screen with maximized emacs.
(setq frame-resize-pixelwise t) ; emacs.stackexchange.com/questions/34675
;; Increases font in default buffer, which all other buffers base off of. May be overwritten by buffer specific fonts
(set-face-attribute 'default nil :height 110) ; stackoverflow.com/questions/294664
;; Consider fixing commenting with https://stackoverflow.com/questions/26312317
;; Display line numbers on all files. Currently includes shell, want to disable that
;; https://emacs.stackexchange.com/questions/36747 likely has the solution
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-frame-parameter nil 'fullscreen 'fullboth) ; Start emacs fullscreen, don't toggle when evaling buffer (probably best to move to init hook)
(scroll-bar-mode -1)
(setq Info-hide-note-references 1)
;; Inhibit startup message
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t			                ; don't clobber symlinks
   backup-directory-alist				;
    `(("." . ,(concat user-emacs-directory "backups")))	; don't litter my fs tree
   delete-old-versions t				;
   kept-new-versions 6					;
   kept-old-versions 2					;
   version-control t)			                ; use versioned backups
(delete-selection-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)


(provide 'init)
;;; init.el ends here
