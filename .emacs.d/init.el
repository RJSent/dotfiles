;;; init.el --- Initialization
;; This file replaces itself with the actual configuration at first run.

;; We can't tangle without org!

;;; Commentary:
;; 

(require 'org)
(require 'org-crypt)
;; Open the configuration
;;; Code:

(find-file "~/config.org")
;; tangle it
(org-decrypt-entries)
(org-babel-tangle)
(org-encrypt-entries)
;; load it
(load-file "~/config.org")
;; finally byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))

(provide 'init)

;;; init.el ends here
