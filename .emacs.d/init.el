;;; init.el --- Emacs config initialization
;;
;;; Commentary:
;; 
;;
;;; Code:

;; Setup straight so we can use the most recent org version ASAP
;; Don't want to org-babel load file with an older org version,
;; then use straight to install new version. (I think?)
(eval-and-compile
  (defvar straight-fix-flycheck t)
  (defvar bootstrap-version)
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
  (straight-use-package 'use-package))      ; Install use-package
(setq straight-use-package-by-default t)    ; I don't want to type :straight t a billion times

(straight-use-package 'org)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(provide 'init)

;;; init.el ends here
