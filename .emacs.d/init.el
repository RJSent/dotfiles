;;; init.el --- Initialization
;; This file replaces itself with the actual configuration at first run.

;; We can't tangle without org!

;;; Commentary:
;; 

(require 'org)
;; Open the configuration
;;; Code:

(find-file (concat user-emacs-directory "init.org"))
;; tangle it
(org-babel-tangle)
;; load it
(load-file (concat user-emacs-directory "init.el"))
;; finally byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))

(provide 'init)

;;; init.el ends here
