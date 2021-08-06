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

;; https://www.emacswiki.org/emacs/WhiteSpace
;; https://www.emacswiki.org/emacs?action=browse;oldid=WhitespaceMode;id=WhiteSpace
(setq whitespace-style '(empty face lines-tail tabs trailing))

;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

;; http://ergoemacs.org/emacs/whitespace-mode.html
(global-whitespace-mode nil)

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

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
(use-package server
  :straight nil
  :config
  ;;(progn
  ;;  (defun server-enable ()
  ;;    (unless (server-running-p)
  ;;      (server-start)))
  ;;  (add-hook 'after-init-hook 'server-enable t))
  (server-mode 1))

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
  ;; :hook
  ;; ;; https://github.com/patrickt/emacs
  ;; ((prog-mode) . rainbow-delimiters-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; https://github.com/emacs-evil/evil
;; https://github.com/noctuid/evil-guide
(use-package evil
  :straight
  (evil
    :type git
    :host github
    :repo "emacs-evil/evil")
  :after
  undo-fu
  :init
  ;; https://github.com/emacs-evil/evil-collection#installation
  ;; pre-set some evil vars prior to package load
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'info-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  )

;; https://github.com/joostkremers/visual-fill-column
(use-package visual-fill-column
  :straight
  (visual-fill-column
    :type git
    :host github
    :repo "joostkremers/visual-fill-column")
  :config
  ;;(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  )

;; https://elpa.gnu.org/packages/adaptive-wrap.html
(use-package adaptive-wrap
  :straight
  (adaptive-wrap
   :type git
   :host github
   :repo "emacs-straight/adaptive-wrap")
  :config
  (adaptive-wrap-prefix-mode))

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :straight
  (evil-collection
    :type git
    :host github
    :repo "emacs-evil/evil-collection")
  :after evil
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
  (after-init . global-emojify-mode)
  :config
  ;; https://github.com/twitter/twemoji
  ;; Copy or symlink the twemoji/assets to ~/.emacs.d/emojis/twemoji
  (setq emojify-emoji-set "twemoji"))

;; https://orgmode.org/manual/Hard-indentation.html
(setq org-adapt-indentation nil
      org-hide-leading-stars nil
      org-odd-levels-only nil)

;; https://orgmode.org/worg/org-contrib/org-protocol.html
(use-package org
  :straight nil
  :config
  (require 'org-protocol)
  ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (global-set-key (kbd "C-c c") 'org-capture)
  ;; https://orgmode.org/manual/Structure-Templates.html
  (load-library "org-tempo")
  ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
  ;; activate dot
  (setq org-plantuml-exec-mode 'plantuml)
  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t) (plantuml . t)))
  ;; https://www.reddit.com/r/emacs/comments/ldiryk/weird_tab_behavior_in_org_mode_source_blocks
  (setq org-src-preserve-indentation t)
  (setq
   ;;org-protocol-default-template-key "l"
   ;; %i initial content (region)
   ;; %a annotation
   ;; %c current kill ring head
   ;; TODO: Move to personal.el
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

;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :straight
  (markdown-mode
    :type git
    :host github
    :repo "jrblevin/markdown-mode")
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :straight
  (plantuml-mode
    :type git
    :host github
    :repo "skuro/plantuml-mode")
  :init
  (setq plantuml-default-exec-mode 'executable))

;; https://github.com/nobiot/md-roam
(use-package md-roam
  :straight
  (md-roam
    :type git
    :host github
    :repo "nobiot/md-roam")
  :init
  (setq md-roam-use-markdown-file-links t)
  (setq md_roam-file_extension-single "md")
  (setq org-roam-tag-sources '(prop md-frontmatter))
  (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias))))

;; https://github.com/org-roam/org-roam
(use-package org-roam
  :straight
  (org-roam
    :type git
    :host github
    :repo "org-roam/org-roam")
  :after
  org
  org-protocol
  :init
  (make-directory (file-truename "~/org/roam/") (file-truename "~/org/"))
  (setq org-roam-file-extensions '("org" "md")
        org-roam-directory (file-truename "~/org/roam/")
        org-roam-db-location (file-truename "~/org/roam/org-roam.db")
        org-roam-buffer-width 0.20)
  :hook
  (after-init . org-roam-mode)
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

(use-package org-roam-server
  :straight
  (org-roam-server
    :type git
    :host github
    :repo "org-roam/org-roam-server")
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 6003
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

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

;; https://git.notmuchmail.org/git/notmuch
;; https://github.com/leotaku/literate-emacs/blob/master/init.org#notmuch
;; https://www.reddit.com/r/emacs/comments/ebite6/mu4e_vs_gnus_vs_notmuch_for_emacs_email/
(use-package notmuch
  :straight nil
  :init
  (evil-collection-notmuch-setup))

;; https://github.com/akermu/emacs-libvterm
(use-package vterm :straight nil)

;; https://github.com/politza/pdf-tools
(use-package pdf-tools :straight nil)

;; https://emacs-lsp.github.io/lsp-mode/page/installation/#vanilla-emacs
(use-package lsp-mode
  :straight
  (lsp-mode
    :type git
    :host github
    :repo "emacs-lsp/lsp-mode")
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands
  (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

;; https://github.com/jkitchin/ox-clip
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
(use-package ox-clip
  :straight
  (ox-clip
    :type git
    :host github
    :repo "jkitchin/ox-clip"))

;; https://github.com/bbatsov/projectile/
(use-package projectile
  :straight
  (projectile
    :type git
    :host github
    :repo "bbatsov/projectile"))

;; https://github.com/bastibe/annotate.el
(use-package annotate
  :straight
  (annotate
    :type git
    :host github
    :repo "bastibe/annotate.el"))

;; https://github.com/abo-abo/swiper
(use-package swiper
  :straight
  (swiper
    :type git
    :host github
    :repo "abo-abo/swiper")
  :config
  (straight-use-package 'counsel)
  (ivy-mode)
  (counsel-mode)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

(load "~/.emacs.d/personal.el")

;; https://github.com/emacs-lsp/lsp-docker
(use-package lsp-docker
  :straight
  (lsp-docker
   :type git
   :host github
   :repo "emacs-lsp/lsp-docker")
  :init
  (setq lsp-docker-client-packages '()
        lsp-docker-client-configs '())
  :config
  (lsp-docker-init-clients
   :path-mappings '(())
   :client-packages lsp-docker-client-packages
   :client-configs lsp-docker-client-configs))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
