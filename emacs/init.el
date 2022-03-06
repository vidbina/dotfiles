;; https://github.com/raxod502/straight.el/issues/757#issuecomment-839764260
(defvar comp-deferred-compilation-deny-list ())

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

;; https://orgmode.org/worg/org-contrib/org-protocol.html
;; https://github.com/org-roam/org-roam/issues/529
;; https://git.savannah.gnu.org/cgit/emacs/org-mode.git/
(use-package org
  :straight (:type built-in)
  :init
  (setq org-adapt-indentation nil ; https://orgmode.org/manual/Hard-indentation.html
        org-hide-leading-stars nil
        org-odd-levels-only nil)
  :config
  ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c d") 'org-hide-drawer-toggle)
  ;; https://orgmode.org/manual/Structure-Templates.html
  (require 'org-tempo)
  ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
  (setq org-plantuml-exec-mode 'plantuml)
  ;; https://www.reddit.com/r/emacs/comments/ldiryk/weird_tab_behavior_in_org_mode_source_blocks
  (setq org-src-preserve-indentation t
        org-hide-block-startup t)
  :custom
  (org-tags-column 0 "Avoid wrapping issues by minimizing tag indentation"))

;; https://github.com/jkitchin/ox-clip
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
(use-package ox-clip
  :straight (ox-clip :type git
                     :host github
                     :repo "jkitchin/ox-clip")
  :after org
  :bind
  ("C-c y" . ox-clip-formatted-copy))

(use-package ol-bibtex
  :straight (:type built-in)
  :after org
  :custom
  (org-bibtex-prefix "BIB_" "Define prefix for arbitrary fields")
  (org-bibtex-export-arbitrary-fields t "Export prefixed fields"))

;; https://git.sr.ht/~bzg/org-contrib
(use-package org-contrib
  :straight (org-contrib :type git
                         :host nil
                         :repo "https://git.sr.ht/~bzg/org-contrib")
  :after org)

;; https://github.com/org-roam/org-roam
(use-package org-roam
  :straight (org-roam :type git
                      :host github
                      :repo "org-roam/org-roam")
  :after org
  :init
  (setq org-roam-v2-ack t)
  (make-directory (file-truename "~/org/roam/") t)
  :custom
  (org-roam-file-extensions '("org" "md"))
  (org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-db-location (file-truename "~/org/roam/org-roam.db"))
  :config
  (message "📔 org-roam is loaded")
  (org-roam-db-autosync-mode 1)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)))

;; https://github.com/org-roam/org-roam-ui
(use-package org-roam-ui
  :delight
  (org-roam-ui-mode "🕸️")
  (org-roam-ui-follow-mode "👀")
  :straight (org-roam-ui :host github
                         :repo "org-roam/org-roam-ui"
                         :branch "main"
                         :files ("*.el" "out"))
  :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; a hookable mode anymore, you're advised to pick something yourself
  ;; if you don't care about startup time, use
  :bind (("C-c n ." . org-roam-ui-node-zoom)
         ("C-c n ," . org-roam-ui-node-local))
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow nil
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; https://github.com/nobiot/md-roam
(use-package md-roam
  :straight (md-roam :type git
                     :host github
                     :repo "nobiot/md-roam")
  :after org-roam
  :init
  (setq md-roam-use-markdown-file-links t
        md-roam-file_extension-single "md"
        org-roam-tag-sources '(prop md-frontmatter)
        org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias))))

;; https://github.com/org-roam/org-roam-bibtex
(use-package org-roam-bibtex
  :straight (org-roam-bibtex :type git
                             :host github
                             :repo "org-roam/org-roam-bibtex")
  :after org-roam)
;;:config
;;(require 'org-ref)
;;:custom
;;(orb-roam-ref-format 'org-ref-v3 "Use new org-ref cite:&links notation in ROAM_REFS property"))

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

;; https://www.emacswiki.org/emacs/WhiteSpace
;; https://www.emacswiki.org/emacs?action=browse;oldid=WhitespaceMode;id=WhiteSpace
(setq whitespace-style '(empty face lines-tail tabs trailing))

;; http://ergoemacs.org/emacs/whitespace-mode.html
(global-whitespace-mode nil)

(use-package delight
  :straight (delight :type git
                     :host nil
                     :repo "https://git.savannah.nongnu.org/git/delight.git")
  :delight
  (auto-revert-mode "♻️")
  (eldoc-mode " el📖")
  (edebug-mode "🐞")
  (global-whitespace-mode)
  (visual-line-mode " 🌯")
  (mu4e-main-mode "📫")
  (mu4e-headers-mode "📬")
  (mu4e-view-mode "📧"))

(use-package diminish
  :disabled
  :straight (diminish :type git
                      :host github
                      :repo "myrjola/diminish.el"))

;; https://github.com/joostkremers/visual-fill-column
(use-package visual-fill-column
  :straight (visual-fill-column :type git
                                :host github
                                :repo "joostkremers/visual-fill-column"))

;; https://elpa.gnu.org/packages/adaptive-wrap.html
(use-package adaptive-wrap
  :straight (adaptive-wrap :type git
                           :host github
                           :repo "emacs-straight/adaptive-wrap")
  :config
  (adaptive-wrap-prefix-mode))

;; https://github.com/purcell/default-text-scale
;; Doesn't work well in emacsclient
(use-package default-text-scale
  :straight (default-text-scale :type git
                                :host github
                                :repo "purcell/default-text-scale")
  :hook
  (after-init . default-text-scale-mode)
  :init
  (add-hook 'server-after-make-frame-hook
            (lambda () (progn (message "🎨 Time to dress up the GUI")
                              (default-text-scale-reset)))))

;; https://gitlab.com/protesilaos/modus-themes
(use-package modus-themes
  :straight (modus-themes :type git
                          :host gitlab
                          :repo "protesilaos/modus-themes")
  :config
  (modus-themes-load-themes)
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-mode-line '(3d accented)
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

(setq display-buffer-alist
      (let* ((sidebar-width '(window-width . 85))
             (sidebar-parameters '(window-parameters . ((no-other-window . t))))
             (sidebar (list '(side . left) sidebar-width sidebar-parameters)))
        (list (cons (regexp-opt-group '("*org-roam*"))
                    (cons #'display-buffer-in-side-window
                          `((slot . 0) ,@sidebar)))
              (cons (regexp-opt-group '("*Dictionary*"))
                    (cons #'display-buffer-in-side-window
                          `((slot . -1) ,@sidebar)))
              (cons (regexp-opt-group '("*Help*" "*Info*" "*info*"))
                    (cons #'display-buffer-in-side-window
                          `((slot . 5) ,@sidebar)))
              (cons (regexp-opt-group '("*Shortdoc"))
                    (cons #'display-buffer-in-side-window
                          `((slot . 6) ,@sidebar)))
              (cons (regexp-opt-group '("*Warnings*"))
                    (cons #'display-buffer-in-side-window
                          `((slot . 10) ,@sidebar))))))

;; https://github.com/emacsorphanage/zoom-window
(use-package zoom-window
  :straight (zoom-window :type git
                         :host github
                         :repo "emacsorphanage/zoom-window")
  :init
  (setq ;;zoom-window-use-persp t
   zoom-window-mode-line-color "DarkRed")
  :config
  (global-set-key (kbd "C-c C-z") 'zoom-window-zoom))

;; https://github.com/abo-abo/ace-window
;; https://jao.io/blog/2020-05-12-ace-window.html
(use-package ace-window
  :straight (ace-window :type git
                        :host github
                        :repo "abo-abo/ace-window")
  :bind (("M-o" . ace-window)))

;; https://github.com/emacsorphanage/dired-k
(use-package dired-k
  :straight (dired-k :type git
                     :host github
                     :repo "emacsorphanage/dired-k")
  :init
  (setq dired-k-style 'git)
  :config
  (add-hook 'dired-initial-position-hook 'dired-k))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html#index-ffap
(ffap-bindings)

;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

;; https://github.com/emacsmirror/undo-fu
(use-package undo-fu
  :straight (undo-fu :type git
                     :host github
                     :repo "emacsmirror/undo-fu"))

;; https://github.com/jwiegley/emacs-async
(use-package async
  :straight (async :type git
                   :host github
                   :repo "jwiegley/emacs-async"))

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
  (evil-collection-init)
  :delight
  (evil-collection-unimpaired-mode))

;; https://github.com/magit/magit.git
(use-package magit
  :straight (magit :type git
                   :host github
                   :repo "magit/magit"))

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :straight (diff-hl :type git
                     :host github
                     :repo "dgutov/diff-hl")
  :hook
  (after-init . global-diff-hl-mode))

;; https://github.com/jrblevin/deft
(use-package deft
  :straight (deft :type git
                  :host github
                  :repo "jrblevin/deft")
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-directory "~/org")
  (deft-extensions '("md" "org"))
  (deft-recursive t)
  (deft-strip-summary-regexp
   (concat "\\("
           "[\n\t]" ;; blank
           "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
           "\\)"))
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t))

(use-package orderless
  :straight (orderless :type git
                       :host github
                       :repo "oantolin/orderless")
  :custom
  (completion-styles '(orderless))
  (ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

;; https://github.com/abo-abo/swiper
(use-package swiper
  :straight (swiper :type git
                    :host github
                    :repo "abo-abo/swiper")
  :delight
  (counsel-mode)
  (ivy-mode)
  :config
  (straight-use-package 'counsel)
  (ivy-mode)
  (counsel-mode)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :delight
  :straight (which-key :type git
                       :host github
                       :repo "justbur/emacs-which-key")
  :config
  (which-key-mode))

;; https://www.djcbsoftware.nl/code/mu/mu4e.html
(use-package mu4e
  :straight (:type built-in)
  :config
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-compose-format-flowed t
        mu4e-context-policy 'always-ask
        mu4e-get-mail-command "true"
        mu4e-index-update-in-background t
        mu4e-view-show-addresses t))

;; https://git.notmuchmail.org/git/notmuch
;; https://github.com/leotaku/literate-emacs/blob/master/init.org#notmuch
;; https://www.reddit.com/r/emacs/comments/ebite6/mu4e_vs_gnus_vs_notmuch_for_emacs_email/
(use-package notmuch
  :straight (:type built-in)
  :init
  (evil-collection-notmuch-setup)
  :config
  (notmuch-address-harvest))

(use-package pdf-tools
  :straight nil
  :config
  (require 'pdf-occur)
  (pdf-tools-install nil t nil nil)
  (setq-default pdf-view-display-size 'fit-width))

;; https://github.com/bbatsov/projectile/
(use-package projectile
  :straight (projectile :type git
                        :host github
                        :repo "bbatsov/projectile")
  :custom
  (projectile-mode-line-prefix "🗄️"))

;; https://github.com/Bad-ptr/persp-mode.el
(use-package persp-mode
  :straight (persp-mode :type git
                        :host github
                        :repo "Bad-ptr/persp-mode.el")
  :config
  (persp-mode t))

(load "~/.emacs.d/lang.el")
(load "~/.emacs.d/personal.el")

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
