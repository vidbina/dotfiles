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

;; TODO: Reeval if this compat hack is still needed
;; Issue is that straight relies on vars with prefixes that have been
;; renamed from comp to native-comp on the Emacs side.
;; https://github.com/raxod502/straight.el/issues/757#issuecomment-839764260
(defvar comp-deferred-compilation-deny-list ())

;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

;;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
;;;; https://www.emacswiki.org/emacs/EmacsClient
;;(use-package server
;;  :straight nil
;;  :config
;;  (server-mode))

;; https://github.com/jwiegley/emacs-async
(use-package async
  :straight (async :type git
                   :host github
                   :repo "jwiegley/emacs-async"))

;; https://github.com/emacsmirror/undo-fu
(use-package undo-fu
  :straight (undo-fu :type git
                     :host github
                     :repo "emacsmirror/undo-fu"))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight (rainbow-delimiters :type git
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
  :straight (evil :type git
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
  (evil-mode t)
  (evil-set-initial-state 'info-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs))

;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :straight (evil-collection :type git
                             :host github
                             :repo "emacs-evil/evil-collection")
  :after evil
  :config
  (evil-collection-init))

;;;; https://github.com/joostkremers/visual-fill-column
;;(use-package visual-fill-column
;;  :straight (visual-fill-column :type git
;;                                :host github
;;                                :repo "joostkremers/visual-fill-column"))

;;; https://elpa.gnu.org/packages/adaptive-wrap.html
;;(use-package adaptive-wrap
;;  :straight (adaptive-wrap :type git
;;                           :host github
;;                           :repo "emacs-straight/adaptive-wrap")
;;  :config
;;  (adaptive-wrap-prefix-mode))

;; https://github.com/purcell/default-text-scale
;; Doesn't work well in emacsclient
(use-package default-text-scale
  :straight (default-text-scale :type git
                                :host github
                                :repo "purcell/default-text-scale")
  :hook
  (after-init . default-text-scale-mode))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight (which-key :type git
                       :host github
                       :repo "justbur/emacs-which-key")
  :config
  (which-key-mode))

;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :straight (emojify :type git
                     :host github
                     :repo "iqbalansari/emacs-emojify")
  :hook
  (after-init . global-emojify-mode)
  :config
  ;; https://github.com/twitter/twemoji
  ;; Copy or symlink the twemoji/assets to ~/.emacs.d/emojis/twemoji
  ;;(setq emojify-emoji-set "twemoji-v2")
  (setq emojify-composed-text-p nil
        emojify-emoji-set "emojione-v2.2.6")
  :bind
  ("C-c e" . emojify-insert-emoji))

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
  (global-set-key (kbd "C-c d") 'org-hide-drawer-toggle)
  ;; https://orgmode.org/manual/Structure-Templates.html
  (load-library "org-tempo")
  ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
  ;; activate dot
  (setq org-plantuml-exec-mode 'plantuml)
  ;; https://www.reddit.com/r/emacs/comments/ldiryk/weird_tab_behavior_in_org_mode_source_blocks
  (setq org-src-preserve-indentation t
        org-hide-block-startup t
        org-capture-templates '(("w" "Default Template" entry (file+headline "~/org/protocol/capture.org" "Notes") "* %^{Title}\n\nSource: %u, %c\n\n%i" :empty-lines 1)
                                ("p" "Link with Selected Text" entry (file+headline "~/org/protocol/capture.org" "Links") "* TODO Read %^{title}\n\n Source: %:annotation\n\n #+BEGIN_QUOTE\n\n %i\n\n #+END_QUOTE%?" :empty-lines 2)
                                ("L" "Link Only" entry (file+headline "~/org/protocol/capture.org" "Links") "* TODO Read _%:description_\n\nSource: %:annotation%?" :empty-lines 2)
                                ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks") "* TODO %?\n\n%i\n\n%a"))))

;; https://github.com/nobiot/md-roam
(use-package md-roam
  :straight (md-roam :type git
                     :host github
                     :repo "nobiot/md-roam")
  :init
  (setq md-roam-use-markdown-file-links t
        md-roam-file_extension-single "md"
        org-roam-tag-sources '(prop md-frontmatter)
        org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias))))

;; https://github.com/org-roam/org-roam
(use-package org-roam
  :straight (org-roam :type git
                      :host github
                      :repo "org-roam/org-roam")
  :after
  org
  org-protocol
  :init
  (make-directory (file-truename "~/org/roam/")
                  (file-truename "~/org/"))
  (setq org-roam-file-extensions '("org" "md")
        org-roam-directory (file-truename "~/org/roam/")
        org-roam-db-location (file-truename "~/org/roam/org-roam.db")
        org-roam-v2-ack t
        org-roam-buffer-width 0.20)
  ;;:config
  ;;(require 'org-roam-protocol)
  :hook
  (after-init . org-roam-mode)
  :bind
  (:map org-roam-mode-map (("C-c n l" . org-roam)
                           ("C-c n f" . org-roam-find-file)
                           ("C-c n g" . org-roam-graph)))
  (:map org-mode-map (("C-c n i" . org-roam-insert)
                      ("C-c n I" . org-roam-insert-immediate))))

;; https://github.com/org-roam/org-roam-ui
(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :straight (diff-hl :type git
                     :host github
                     :repo "dgutov/diff-hl")
  :hook
  (after-init . global-diff-hl-mode))

;; https://github.com/emacsorphanage/dired-k
(use-package dired-k
  :straight (dired-k :type git
                     :host github
                     :repo "emacsorphanage/dired-k")
  :init
  (setq dired-k-style 'git)
  :config
  (add-hook 'dired-initial-position-hook 'dired-k))

;; https://github.com/jrblevin/deft
(use-package deft
  :straight (deft :type git
                  :host github
                  :repo "jrblevin/deft")
  :config
  (setq deft-directory "~/org"
        deft-extensions '("md" "org")
        deft-recursive t))

;;;; http://company-mode.github.io/
;;(use-package company
;;  :straight (company :type git
;;                     :host github
;;                     :repo "company-mode/company-mode")
;;  :config
;;  (add-hook 'after-init-hook 'global-company-mode)
;;  (define-key company-mode-map (kbd "TAB") #'company-indent-or-complete-common))
;;
;;;; https://git.notmuchmail.org/git/notmuch
;;;; https://github.com/leotaku/literate-emacs/blob/master/init.org#notmuch
;;;; https://www.reddit.com/r/emacs/comments/ebite6/mu4e_vs_gnus_vs_notmuch_for_emacs_email/
;;(use-package notmuch
;;  :straight nil
;;  :init
;;  (evil-collection-notmuch-setup))
;;
;term
;;(use-package vterm :straight nil)
;;
;term
;;(use-package multi-vterm
;;  :straight (multi-vterm :type git
;;                         :host github
;;                         :repo "suonlight/multi-vterm")
;;  :config
;;  ;;(add-hook 'vterm-mode-hook
;;  ;;          (lambda ()
;;  ;;            (setq-local evil-insert-state-cursor 'box)
;;  ;;            (evil-insert-state)))
;;  (define-key vterm-mode-map [return]                      #'vterm-send-return)
;;
;;  (setq vterm-keymap-exceptions nil)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
;;  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
;;  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
;;  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
;;  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
;;  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
;;  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
;;  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
;;  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))


;; https://github.com/politza/pdf-tools
(use-package pdf-tools :straight nil)

;; https://github.com/jkitchin/ox-clip
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
;; to copy org-mode file into HTML for rich-text input controls
(use-package ox-clip
  :straight (ox-clip :type git
                     :host github
                     :repo "jkitchin/ox-clip")
  :bind
  ("C-c y" . ox-clip-formatted-copy))

;; https://github.com/bbatsov/projectile/
(use-package projectile
  :straight (projectile :type git
                        :host github
                        :repo "bbatsov/projectile"))

;; https://github.com/nex3/perspective-el
(use-package perspective
  :straight (perspective :type git
                         :host github
                         :repo "nex3/perspective-el")
  :bind (("C-x C-b" . persp-ivy-switch-buffer)
         ("C-x k" . persp-kill-buffer*))
  :config
  (persp-mode t)
  :init
  (setq persp-state-default-file "~/.emacs.d/perspective"
        persp-modestring-short t))

;; https://github.com/emacsorphanage/zoom-window
(use-package zoom-window
  :straight (zoom-window :type git
                         :host github
                         :repo "emacsorphanage/zoom-window")
  :init
  (setq ;;zoom-window-use-persp t
   zoom-window-mode-line-color "LightPink")
  :config
  (global-set-key (kbd "C-c C-z") 'zoom-window-zoom))

;; https://github.com/abo-abo/swiper
(use-package swiper
  :straight (swiper :type git
                    :host github
                    :repo "abo-abo/swiper")
  :config
  (straight-use-package 'counsel)
  (ivy-mode)
  (counsel-mode)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

;; https://github.com/fniessen/emacs-leuven-theme
(use-package leuven-theme
  :straight (leuven-theme :type git
                          :host github
                          :repo "fniessen/emacs-leuven-theme"))

;; https://gitlab.com/protesilaos/modus-themes
(use-package modus-themes
  :straight (modus-themes :type git
                          :host gitlab
                          :repo "protesilaos/modus-themes")
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi)
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-mode-line '(moody accented borderless)
        modus-themes-org-blocks 'gray-background
        modus-themes-region '(bg-only no-extend accented)
        modus-themes-prompts '(intense)
        modus-themes-fringes '(intense)
        modus-themes-hl-line '(accented)
        modus-themes-paren-match '(bold intense)
        modus-themes-syntax '(yellow-comments green-strings alt-syntax)
        modus-themes-headings '((1 . (background overline))
                                (2 . (background overline rainbow))
                                (t . (background overline rainbow)))
        modus-themes-scale-headings t))

(load "~/.emacs.d/lang.el")
(load "~/.emacs.d/personal.el")

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
