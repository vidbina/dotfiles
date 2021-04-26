;; Format with indent-region

;; https://www.emacswiki.org/emacs/ScrollBar
(scroll-bar-mode -1)

;; https://www.emacswiki.org/emacs/ToolBar
(tool-bar-mode -1)

;; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode -1)

;; https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

;; https://www.emacswiki.org/emacs/FillColumnIndicator
(global-display-fill-column-indicator-mode 1)

;; https://www.emacswiki.org/emacs/LineNumbers
(global-display-line-numbers-mode 1)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html#index-ffap
;; =describe-package ffap=
(ffap-bindings)

;; https://www.emacswiki.org/emacs?action=browse;oldid=WhitespaceMode;id=WhiteSpace
(setq whitespace-style '(empty face lines-tail tabs trailing))

;; https://www.reddit.com/r/emacs/comments/643dkt/use_package_vs_require_and_maybe_some_sorrowful/dfz3mtx

;; https://orgmode.org/worg/org-contrib/org-protocol.html
;; https://orgmode.org/worg/org-faq.html#mixed-install
(add-to-list 'load-path "~/.emacs.d/straight/build/org")

;; https://github.com/raxod502/straight.el#getting-started
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

;; https://github.com/raxod502/straight.el#integration-with-use-package
(straight-use-package 'use-package)

(use-package server
  :straight nil
  :config
  (progn
    (defun server-enable ()
      (unless (server-running-p)
        (server-start)))
    (add-hook 'after-init-hook 'server-enable t)))
;;(server-start)

;; https://github.com/emacsmirror/undo-fu
(use-package undo-fu
  :straight
  (undo-fu
    :type git
    :host github
    :repo "emacsmirror/undo-fu"))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight
  (rainbow-delimiters
    :type git
    :host github
    :repo "Fanael/rainbow-delimiters")
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; https://github.com/emacs-evil/evil
(use-package evil
  :after
  undo-fu
  :straight
  (evil
    :type git
    :host github
    :repo "emacs-evil/evil")
  :init
  ;; https://github.com/emacs-evil/evil-collection#installation
  ;; pre-set some evil vars prior to package load
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :straight
  (evil-collection
    :type git
    :host github
    :repo "emacs-evil/evil-collection")
  :config
  (evil-collection-init))

;; https://github.com/purcell/default-text-scale
;; Doesn't work well in emacsclient
(use-package default-text-scale
  :straight
  (default-text-scale
    :type git
    :host github
    :repo "purcell/default-text-scale")
    :hook
    (after-init . default-text-scale-mode)
  ;; :config
  ;; (default-text-scale-mode)
  )

;; https://github.com/hlissner/emacs-doom-themes#manually
(use-package doom-themes
  :straight
  (doom-themes
    :type git
    :host github
    :repo "hlissner/emacs-doom-themes")
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-molokai-comment-bg t)
  ;;(setq doom-molokai-brighter-comments t)
  (setq doom-molokai-brighter-modeline t)
  ;; https://github.com/hlissner/emacs-doom-themes/tree/screenshots
  (load-theme 'doom-molokai t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight
  (which-key
    :type git
    :host github
    :repo "justbur/emacs-which-key")
  :config
  (which-key-mode))

;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :straight
  (emojify
    :type git
    :host github
    :repo "iqbalansari/emacs-emojify")
  :hook
  (after-init . global-emojify-mode))

;; https://orgmode.org/worg/org-contrib/org-protocol.html
(use-package org
  :straight nil
  :config
  (require 'org-protocol)
  ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq
   ;;org-protocol-default-template-key "l"
   ;; %i initial content (region)
   ;; %a annotation
   ;; %c current kill ring head
   org-capture-templates '(
    ("w" "Default Template"
     entry (file+headline "~/org/protocol/capture.org" "Notes")
     "* %^{Title}\n\nSource: %u, %c\n\n%i"
     :empty-lines 1)
    ("p" "Link with Selected Text"
     entry (file+headline "~/org/protocol/capture.org" "Links")
     "* TODO Read %^{title}\n\n Source: %:annotation\n\n #+BEGIN_QUOTE\n\n %i\n\n #+END_QUOTE%?"
     :empty-lines 2)
    ("L" "Link Only"
     entry (file+headline "~/org/protocol/capture.org" "Links")
     "* TODO Read _%:description_\n\nSource: %:annotation%?"
     :empty-lines 2)
    ("t" "Todo"
     entry (file+headline "~/org/todo.org" "Tasks")
     "* TODO %?\n\n%i\n\n%a")
    ;; ..
    )))

;; https://github.com/org-roam/org-roam
(use-package org-roam
  :after
  org
  org-protocol
  :straight
  (org-roam
    :type git
    :host github
    :repo "org-roam/org-roam")
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/roam")
  :bind
  (:map org-roam-mode-map
        (("C-c n l" . org-roam)
         ("C-c n f" . org-roam-find-file)
         ("C-c n g" . org-roam-graph)))
  (:map org-mode-map
        (("C-c n i" . org-roam-insert)
         ("C-c n I" . org-roam-insert-immediate))))

(use-package org-roam-protocol
  :straight
  :after
  org-protocol
  org-roam
  )

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :straight
  (diff-hl
   :type git
   :host github
   :repo "dgutov/diff-hl")
  :hook
  (after-init . global-diff-hl-mode))

;; https://github.com/emacsorphanage/dired-k
(use-package dired-k
  :straight
  (dired-k
    :type git
    :host github
    :repo "emacsorphanage/dired-k")
  :init
  (setq dired-k-style 'git)
  :config
  (add-hook 'dired-initial-position-hook 'dired-k))

;; https://github.com/jrblevin/deft
(use-package deft
  :straight
  (deft
    :type git
    :host github
    :repo "jrblevin/deft")
  :config
  (setq deft-directory "~/org"
        deft-extensions '("md" "org")
        deft-recursive t))

;; http://company-mode.github.io/
(use-package company
  :straight
  (company
    :type git
    :host github
    :repo "company-mode/company-mode")
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-mode-map (kbd "TAB") #'company-indent-or-complete-common))

;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :straight nil)

;; https://orgmode.org/manual/Structure-Templates.html
(load-library "org-tempo")

(load "~/.emacs.d/personal.el")

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
