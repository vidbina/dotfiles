; https://github.com/raxod502/straight.el#getting-started
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

; https://github.com/emacs-evil/evil
(use-package evil
  :straight (evil
              :type git
              :host github
              :repo "emacs-evil/evil")
  :init
  ; https://github.com/emacs-evil/evil-collection#installation
  ; pre-set some evil vars prior to package load
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :straight (evil-collection
	     :type git
	     :host github
	     :repo "emacs-evil/evil-collection")
  :config
  (evil-collection-init))

(use-package srefactor
  :straight (srefactor
	     :type git
	     :host github
	     :repo "tuhdo/semantic-refactor"))

; https://github.com/hlissner/emacs-doom-themes#manually
(use-package doom-themes
  :straight (doom-themes
              :type git
              :host github
              :repo "hlissner/emacs-doom-themes")
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ; https://github.com/hlissner/emacs-doom-themes/tree/screenshots
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight (which-key
              :type git
              :host github
              :repo "justbur/emacs-which-key")
  :config
  (which-key-mode))

; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :straight (emojify
	     :type git
	     :host github
	     :repo "iqbalansari/emacs-emojify")
  :hook (after-init . global-emojify-mode))

; https://www.emacswiki.org/emacs/ScrollBar
(scroll-bar-mode -1)

; https://www.emacswiki.org/emacs/ToolBar
(tool-bar-mode -1)

; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode -1)

; https://orgmode.org/manual/Structure-Templates.html
(load-library "org-tempo")

(load "~/.emacs.d/personal.el")

; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
