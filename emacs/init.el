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

; https://github.com/raxod502/straight.el#integration-with-use-package
(straight-use-package 'use-package)

(use-package evil
  :straight (evil
              :type git
              :host github
              :repo "emacs-evil/evil"
              :fork (:host github
                     :repo "emacs-evil/evil")))

(require 'evil)
(evil-mode t)
