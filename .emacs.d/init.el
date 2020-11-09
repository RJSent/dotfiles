;;; init.el --- Initialization file for Emacs
;;
;;
;;; Commentary:
;; It's what you'd expect.
;;
;;; Code:


(eval-and-compile                       ; old.reddit.com/r/emacs/comments/gwupwt/noob_please_help_to_resolve_the_flycheck_error/ft1kk2j
  (defvar straight-fix-flycheck t)      ; github.com/raxod502/straight.el#integration-with-flycheck
  (defvar bootstrap-version)            ; Install straight.el
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
  (straight-use-package 'use-package))  ; Install use-package
(setq straight-use-package-by-default t)


;;; Constants. See centaur emacs, init-const.el.

(defconst sys/win32p                    ; Boy, I hope I don't need this constant
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")


;;; Navigation, aesthetic, and other global packages

(use-package ace-window
  :defines aw-keys
  :functions ace-window
  :bind* ("M-o" . 'ace-window) ; * as ibuffer overrides M-o. Consider adjusting as M-o is used for ivy-dispatching done
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
(use-package which-key
  :defines which-key-add-column-padding
  :functions which-key-mode
  :defer 1
  :diminish
  :config (which-key-mode)
  (setq which-key-add-column-padding 3))
(defun aorst/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))
(use-package all-the-icons
  :config
  ;; seagle0128's approach in https://github.com/domtronn/all-the-icons.el/issues/120 attempts to install for terminals
  (when (and (not (aorst/font-installed-p "all-the-icons"))
             (window-system))
    (all-the-icons-install-fonts t)))
(use-package diminish)
(use-package doom-themes
  :config (load-theme 'doom-nord t))
;; (use-package hc-zenburn-theme
;;   :straight t
;;   :config (load-theme 'hc-zenburn t))
(use-package uniquify
  :straight nil
  :config (setq uniquify-buffer-name-style 'forward))
(use-package projectile
  :functions projectile-mode
  :diminish
  :config (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package all-the-icons-ibuffer
  :diminish
  :after all-the-icons)
(use-package ibuffer-vc ; Also consider ibuffer-projectile
  :defines ibuffer-sorting-mode ibuffer-inline-columns ibuffer-formats
  :functions ibuffer-vc-set-filter-groups-by-vc-root ibuffer-do-sort-by-alphabetic
  :after all-the-icons-ibuffer
  :hook (ibuffer . (lambda () (ibuffer-vc-set-filter-groups-by-vc-root) ; Look at combining with custom ibuffer groups with 'biffuer-projectile-generate-filter-groups
                     (unless (eq ibuffer-sorting-mode 'alphabetic)      ; Obviously that's an ibuffer-projectile exclusive, not ibuffer-vc
                       (ibuffer-do-sort-by-alphabetic))))               ; https://emacs.stackexchange.com/questions/2181/ibuffer-how-to-automatically-create-groups-per-project
  :bind ("C-x C-b" . ibuffer)                                           ; shows some of code behind projectile filter groups
  :config
  ;; Use human readable Size column instead of original one
  ;; Code from emacs wiki
  (defun ajv/human-readable-file-sizes-to-bytes (string)
    "Convert a human-readable file size into bytes."
    (interactive)
    (cond
     ((string-suffix-p "G" string t)
      (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "M" string t)
      (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
     ((string-suffix-p "K" string t)
      (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
     (t
      (string-to-number (substring string 0 (- (length string) 1))))
     )
    )
  (defun ajv/bytes-to-human-readable-file-sizes (bytes)
    "Convert number of bytes to human-readable file size."
    (interactive)
    (cond
     ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
     ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
     ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
     ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
     ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
     (t (format "%10d" bytes)))
    )
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size"
           :inline t
           :summarizer
           (lambda (column-strings)
             (let ((total 0))
               (dolist (string column-strings)
                 (setq total
                       ;; like, ewww ...
                       (+ (float (ajv/human-readable-file-sizes-to-bytes string))
                          total)))
               (ajv/bytes-to-human-readable-file-sizes total)))  ;; :summarizer nil
           )
    (ajv/bytes-to-human-readable-file-sizes (buffer-size)))
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
  :hook (after-init . doom-modeline-mode)
  :custom                               ; Could use more use-package-ifying
  (doom-modeline-height 20)             ; To better employ its features.
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
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))
(use-package wgrep
  :defer 1)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


;;; Packages for general purpose programming and editing

(use-package company
  :defines company-minimum-prefix-length company-frontends company-idle-delay
  :diminish
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (setq company-idle-delay 1))
(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode)   ; FIXME not working for enh-ruby-mode although that is part of prog-mode
  :config (require 'smartparens-config)) ; Seems fixed now?
(use-package flycheck
  :functions global-flycheck-mode
  :diminish
  :defer 1
  :config (setq flycheck-emacs-lisp-load-path 'inherit)  ; Fixes "org-mode-map" in comment-dwin-2 from being undefined
  (global-flycheck-mode))                                ; Does not fix issues with functions may not be defined
(use-package comment-dwim-2                          ; FIXME: Bug with enh-ruby-mode. No end of line comments
                                                    ; are inserted. ruby-mode does not have this issue. Will
  :config                                            ; look more into what's causing it.
  (defadvice comment-indent (around comment-indent-with-spaces activate) ; Not the cause of enh-ruby-mode issue
     (let ((orig-indent-tabs-mode indent-tabs-mode))
       (when orig-indent-tabs-mode
         (setq indent-tabs-mode nil))
       ad-do-it
       (when orig-indent-tabs-mode
         (setq indent-tabs-mode t))))
  ;; Disabled for now. I want to remove line comment, keeping the
  ;; end of line comment unless I press M-; again.
  ;; Note though, text is killed, not deleted. You can always just
  ;; use C-e C-y to yank the text back at end of line
  ;; (defun cd2/inline-comment-command () ; this is the function called when you repeat the command
  ;;   ;; do nothing (not killing the end-of-line comment)
  ;;   (setq this-command nil) ; This is just a trick so that the command can still be called indefinitely
  ;;   )
  (define-key org-mode-map (kbd "M-;") 'org-comment-dwim-2)
  :bind ("M-;" . comment-dwim-2))


;;; Packages for org-mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (setq org-log-done 'time)
  (setq org-adapt-indentation nil))     ; Swap to 'headline-data once it works properly. (Tested 11/8/2020)
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets) ; FIXME stars are still visible despite below line
  (setq org-superstar-leading-bullet ?\s))   ; As active line is different than others, stars would be visible without this


;;; Packages for ivy and ivy integration

(use-package ivy
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
  :functions ivy-rich-mode
  :after ivy counsel
  :config (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
(use-package counsel
  :functions counsel-mode
  :diminish
  :after ivy
  :config (counsel-mode))
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))
(use-package all-the-icons-ivy-rich
  :after all-the-icons ivy-rich
  :functions all-the-icons-ivy-rich-mode
  :config (all-the-icons-ivy-rich-mode 1))
(use-package counsel-projectile
  :after (counsel projectile)
  :functions counsel-projectile-mode
  :config (counsel-projectile-mode))


;;; Ruby programming packages

(use-package rbenv
  :diminish
  :hook (enh-ruby-mode . global-rbenv-mode))
;; (use-package enh-ruby-mode ; Disabled due to issues & syntax highlighting issue in comments was inexplicably reverted (???)
;;   :mode "\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\|Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"
;;   :interpreter "ruby")
(use-package inf-ruby ; Latest version has --nomultline by default, but not for bundle console, only ruby
  :hook (enh-ruby-mode . inf-ruby-minor-mode))
(use-package robe
  :defines company-backends
  :diminish
  :hook (enh-ruby-mode . robe-mode)
  :config (push 'company-robe company-backends))
(use-package yaml-mode
  :mode "\\.yml\\'")


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
   backup-by-copying t                                  ; don't clobber symlinks
   backup-directory-alist                               ;
    `(("." . ,(concat user-emacs-directory "backups"))) ; don't litter my fs tree
   delete-old-versions t                                ;
   kept-new-versions 6                                  ;
   kept-old-versions 2                                  ;
   version-control t)                                   ; use versioned backups
(delete-selection-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(setq scroll-preserve-screen-position t)
(setq next-screen-context-lines 6)


(provide 'init)
;;; init.el ends here
