;; Tangled from dotfiles/emacs/README.org

;; -*- lexical-binding: t -*-

(message "🚜 Loading init.el")

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
  :bind (:map org-babel-map ("t" . org-babel-tangle-async))
  :config
  ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c d") 'org-hide-drawer-toggle)
  ;; https://www.reddit.com/r/emacs/comments/ldiryk/weird_tab_behavior_in_org_mode_source_blocks
  (setq org-src-preserve-indentation t
        org-hide-block-startup t)
  (defun org-babel-tangle-async (&optional arg target-file lang-re)
    "Call `org-babel-tangle' asynchronously"
    (interactive "P")
    (message "🧬 Async Org-Babel start tangling %s" buffer-file-name)
    (run-hooks 'org-babel-pre-tangle-hook)
    (async-start `(lambda ()
                    (if (and (stringp ,buffer-file-name)
                             (file-exists-p ,buffer-file-name))
                        (progn
                          (setq exec-path ',exec-path
                                load-path ',load-path
                                enable-local-eval t
                                auto-save-default nil
                                org-babel-pre-tangle-hook '())
                          (package-initialize)
                          (find-file ,(buffer-file-name))
                          (read-only-mode t)
                          (goto-char ,(point))
                          (org-babel-tangle ,arg ,target-file ,lang-re) ; tangle! (ref:org-babel-tangle-call)
                          buffer-file-name)
                      (error "🧬 Async Org-Babel is not visiting a file")))
                 `(lambda (result)
                    (message "🧬 Async Org-Babel tangled %s" result))))
  ;; https://orgmode.org/manual/Structure-Templates.html
  (require 'org-tempo)
  ;; https://www.reddit.com/r/emacs/comments/c1b70i/best_way_to_include_source_code_blocks_in_a_latex/
  (add-to-list 'org-latex-packages-alist '("" "listings" nil))
  ;;(setq org-latex-packages-alist nil)
  ;;(setq org-latex-listings t)
  ;;(setq org-latex-listings-options '(("breaklines" "true")))
  (setq org-latex-listings t)
  (setq org-latex-listings-options
        '(("basicstyle" "\\ttfamily")
          ("breakatwhitespace" "false")
          ("breakautoindent" "true")
          ("breaklines" "true")
          ;;("columns" "[c]fullflexible")
          ("commentstyle" "")
          ("emptylines" "*")
          ("extendedchars" "false")
          ;;("fancyvrb" "true")
          ("firstnumber" "auto")
          ("flexiblecolumns" "false")
          ("frame" "single")
          ("frameround" "tttt")
          ("identifierstyle" "")
          ("keepspaces" "true")
          ("keywordstyle" "")
          ("mathescape" "false")
          ("numbers" "left")
          ("numbers" "none")
          ("numbersep" "5pt")
          ("numberstyle" "\\tiny")
          ("resetmargins" "false")
          ("showlines" "true")
          ("showspaces" "false")
          ("showstringspaces" "false")
          ("showtabs" "true")
          ("stepnumber" "2")
          ("stringstyle" "")
          ("tab" "↹")
          ("tabsize" "4")
          ("texcl" "false")
          ("upquote" "false")))
  :custom
  (org-tags-column 0 "Avoid wrapping issues by minimizing tag indentation")
  (org-catch-invisible-edits 'error "Disable invisible edits")
  (org-src-window-setup 'current-window "Show edit buffer in calling window")
  (org-refile-targets '((nil . (:maxlevel . 3))) "Allow refiling to 3rd level headings")
  (org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE(d)" "CANCELED(@c)")) "Allow fast-selection for my standard TODO states")
  (org-html-head (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />" (expand-file-name "ox-html.css" user-emacs-directory)) "Point to our custom stylesheet"))

;; https://github.com/jkitchin/ox-clip
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
(use-package ox-clip
  :straight (ox-clip :type git
                     :host github
                     :repo "jkitchin/ox-clip")
  :after (org)
  :bind
  ("C-c y" . ox-clip-formatted-copy))

;; https://github.com/misohena/phscroll
(use-package phscroll
  :straight (phscroll :type git
                      :host github
                      :repo "misohena/phscroll")
  :init
  (setq org-startup-truncated nil))

;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :straight (htmlize :type git
                     :host github
                     :branch "fix-face-size-unspecified-head"
                     :repo "vidbina/emacs-htmlize")
  :init
  ;; https://www.reddit.com/r/orgmode/comments/5uj17n/invalid_face_error_when_publishing_org_to_html/
  (setq org-html-htmlize-output-type 'inline-css)
  :custom
  (htmlize-ignore-face-size t))

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
  (let ((directory (file-truename "~/org/roam/")))
    (make-directory directory t)
    (setq org-roam-directory directory
          ;; Define a directory that does not change along with the Org-Roam folder
          vidbina-org-roam-root-directory directory))
  (setq org-roam-file-extensions '("org" "md"))
  (setq org-roam-db-location (file-truename "~/org/roam/org-roam.db"))

  :config
  (message "📔 org-roam is loaded")
  (org-roam-db-autosync-disable)

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n u" . vidbina/org-roam-db-async-forced-sync)))

;; https://github.com/org-roam/org-roam-ui
(use-package org-roam-ui
  :straight (org-roam-ui :host github
                         :repo "org-roam/org-roam-ui"
                         :branch "main"
                         :files ("*.el" "out"))
  :delight
  (org-roam-ui-mode "🕸️")
  (org-roam-ui-follow-mode "👀")
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
  :after org-roam
  ;; NOTE: Using org-ref requires additional configuration
  )

;; https://github.com/alphapapa/org-ql
(use-package org-ql
  :straight (org-ql :type git
                    :host github
                    :repo "alphapapa/org-ql"))

;; https://github.com/vidbina/ob-async
(use-package ob-async
  :straight (ob-async :type git
                      :host github
                      :branch "main"
                      :repo "vidbina/ob-async"))

;; https://www.emacswiki.org/emacs/ScrollBar
(scroll-bar-mode -1)

;; https://www.emacswiki.org/emacs/ToolBar
(tool-bar-mode -1)

;; https://www.emacswiki.org/emacs/MenuBar
(menu-bar-mode -1)

;; https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

;; https://www.emacswiki.org/emacs/LineNumbers
(use-package display-line-numbers
  :straight (:type built-in)

  :config
  (display-line-numbers-mode 0)

  :hook
  (prog-mode . (lambda () (display-line-numbers-mode 1)))
  (notmuch-hello-mode . (lambda () (display-line-numbers-mode 0))))

;; https://www.emacswiki.org/emacs/WhiteSpace
;; https://www.emacswiki.org/emacs?action=browse;oldid=WhitespaceMode;id=WhiteSpace
(setq whitespace-style '(empty face lines-tail tabs trailing))

;; https://git.savannah.nongnu.org/git/delight.git
(use-package delight
  :straight (delight :type git
                     :host nil
                     :repo "https://git.savannah.nongnu.org/git/delight.git")
  :delight
  (fundamental-mode "🗒️")
  (auto-revert-mode "♻️")
  (eldoc-mode "el📖")
  (edebug-mode "🐞")
  (whitespace-mode "🏳️")
  (visual-line-mode "🌯")
  (mu4e-main-mode "📫")
  (mu4e-headers-mode "📬")
  (mu4e-view-mode "📧")
  (vterm-mode "👨🏿‍💻"))

;; https://github.com/myrjola/diminish.el
(use-package diminish
  :straight (diminish :type git
                      :host github
                      :repo "myrjola/diminish.el"))

(customize-set-variable 'auto-revert-mode-text "♻️")

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
  :hook ((after-init . default-text-scale-mode)))

;; https://github.com/rnkn/olivetti.git
(use-package olivetti
  :straight (olivetti :type git
                      :host github
                      :repo "rnkn/olivetti"))

;; https://gitlab.com/protesilaos/modus-themes
(use-package modus-themes
  :straight (modus-themes :type git
                          :host gitlab
                          :repo "protesilaos/modus-themes")
  :config
  (modus-themes-load-themes)
  :init
  (setq modus-themes-bold-constructs t
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

(use-package nano-emacs
  :straight (nano-emacs :type git
                        :host github
                        :repo "rougier/nano-emacs")
  :after (:all modus-themes svg-tag-mode mu4e-dashboard mu4e-thread-folding)
  :config
  (require 'nano-base-colors)
  (require 'nano-faces)
  (require 'nano-mu4e))

(use-package mu4e-dashboard
  :straight (mu4e-dashboard :type git
                            :host github
                            :repo "rougier/mu4e-dashboard"))

(use-package mu4e-thread-folding
  :straight (mu4e-thread-folding :type git
                                 :host github
                                 :repo "rougier/mu4e-thread-folding"))

(use-package svg-tag-mode
  :straight (svg-tag-mode :type git
                          :host github
                          :repo "rougier/svg-tag-mode"))

;; https://github.com/emacsorphanage/dired-k
(use-package dired-k
  :straight (dired-k :type git
                     :host github
                     :repo "emacsorphanage/dired-k")
  :init
  (setq dired-k-style 'git)
  :hook (dired-initial-position-hook . dired-k))

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
  (message "Configuring ‘zoom-window’")
  (with-eval-after-load 'persp-mode
    (message "Configuring ‘zoom-window’ to work with ‘persp-mode’")
    (customize-set-variable 'zoom-window-use-persp t
                            "Use zoom-window with persp-mode")))

;; https://github.com/abo-abo/ace-window
;; https://jao.io/blog/2020-05-12-ace-window.html
(use-package ace-window
  :straight (ace-window :type git
                        :host github
                        :repo "abo-abo/ace-window")
  :bind (("M-o" . ace-window)))

;; https://github.com/abo-abo/avy
(use-package avy
  :straight (avy :type git
                 :host github
                 :repo "abo-abo/avy")
  :bind (("C-:" . avy-goto-char)))

;; https://github.com/daichirata/emacs-rotate
(use-package rotate
  :straight (rotate :type git
                    :host github
                    :repo "daichirata/emacs-rotate"))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html#index-ffap
(ffap-bindings)

;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Indent-Tabs-Mode.html
(setq-default indent-tabs-mode nil)

;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq-default scroll-conservatively 100)

;; https://github.com/emacsmirror/undo-fu
(use-package undo-fu
  :straight (undo-fu :type git
                     :host github
                     :repo "emacsmirror/undo-fu"))

;; https://github.com/jwiegley/emacs-async
(use-package async
  :straight (async :type git
                   :host github
                   :repo "jwiegley/emacs-async")
  :config
  (async-bytecomp-package-mode 1)
  :custom
  (async-variables-noprops-function #'async--purecopy))

;; https://github.com/victorhge/iedit
(use-package iedit
  :straight (iedit :type git
                   :host github
                   :repo "victorhge/iedit"))

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
  (advice-add 'evil-collection-mu4e-setup
              :before (lambda ()
                        (message "😈 Setup up evil-collection for mu4e 📧")))
  :delight
  (evil-collection-unimpaired-mode))

;; https://github.com/alexmurray/evil-vimish-fold
(use-package evil-vimish-fold
  :straight (evil-vimish-fold :type git
                              :host github
                              :repo "alexmurray/evil-vimish-fold")
  :diminish evil-vimish-fold-mode
  :after
  (:all vimish-fold)
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

;; https://github.com/matsievskiysv/vimish-fold
(use-package vimish-fold
  :straight (vimish-fold :type git
                         :host github
                         :repo "matsievskiysv/vimish-fold")
  :after evil)

;; https://github.com/bastibe/annotate.el
(use-package annotate
  :straight (annotate :type git
                      :host github
                      :repo "bastibe/annotate.el")
  :custom
  (annotate-file-buffer-local nil "Use central annotations file"))

;; https://github.com/magit/magit.git
(use-package magit
  :straight (magit :type git
                   :host github
                   :repo "magit/magit")
  :custom
  ;; was previously #'magit-display-buffer-traditional
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
                                 "Open magit buffer in window at point")
  (magit-diff-refine-hunk t "Show fine differences (word-granularity) for current hunk only"))

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :straight (diff-hl :type git
                     :host github
                     :repo "dgutov/diff-hl")
  :hook (after-init . global-diff-hl-mode)
  :custom
  (diff-hl-margin-mode t "Use margin mode to clear up the fringe"))

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
  ;; NOTE: Load Orderless after Swiper when using the Ivy integration
  :custom
  (completion-styles '(orderless)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :straight (marginalia :type git
                        :host github
                        :repo "minad/marginalia")
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; https://github.com/minad/consult
(use-package consult
  :straight (consult :type git
                     :host github
                     :repo "minad/consult")
  :bind
  (;; bindings from https://github.com/minad/consult#use-package-example
   ;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
   ("C-x b"   . consult-buffer)              ; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#"   . consult-register-load)
   ("M-'"   . consult-register-store)        ; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y"      . consult-yank-pop)           ; orig. yank-pop
   ("<help> a" . consult-apropos)            ; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e"   . consult-compile-error)
   ("M-g f"   . consult-flymake)             ; Alternative: consult-flycheck
   ("M-g g"   . consult-goto-line)           ; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ; orig. goto-line
   ("M-g o"   . consult-outline)             ; Alternative: consult-org-heading
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)       ; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ; orig. next-matching-history-element
   ("M-r" . consult-history)                 ; orig. previous-matching-history-element
   )

  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; https://github.com/minad/vertico
(use-package vertico
  :straight (vertico :type git
                     :host github
                     :repo "minad/vertico")
  :init
  (vertico-mode)

  :config
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :straight (:type built-in)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  :hook (minibuffer-setup . cursor-intangible-mode))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight (which-key :type git
                       :host github
                       :repo "justbur/emacs-which-key")
  :delight
  :config
  (which-key-mode))

(use-package vterm
  :straight (:type built-in))

(use-package pdf-tools
  :straight (:type built-in)
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
  (projectile-mode-line-prefix "🗄️")
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map)))

;; https://github.com/Bad-ptr/persp-mode.el
(use-package persp-mode
  :straight (persp-mode :type git
                        :host github
                        :repo "Bad-ptr/persp-mode.el")
  :diminish persp-mode
  :config
  (persp-mode t)
  :custom
  (persp-auto-resume-time 0 "Avoid autoloading perspective")
  (persp-filter-save-buffers-functions
   (list (lambda (b) (string-prefix-p "*" (buffer-name b)))
         (lambda (b) (not (null (string-match-p (rx (seq word-boundary "magit"
                                                         (zero-or-more (seq "-" (one-or-more any))) ":")) (buffer-name b)))))
         (lambda (b) (string-match-p
                      (regexp-opt '("mu4e-compose-mode"))
                      (symbol-name (buffer-local-value 'major-mode b)))))
   "Filter out special and magit buffers from saving"))

;; https://www.djcbsoftware.nl/code/mu/mu4e.html
(use-package mu4e
  :after (:all
          message
          sendmail)
  :straight (:type built-in)
  :demand t
  :bind (("C-c M 4" . mu4e))
  :hook (
         ;; https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html
         (dired-mode . turn-on-gnus-dired-mode))
  :config
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Sample"
             :enter-func (lambda () (mu4e-message "Into SAMPLE mu4e context"))
             :leave-func (lambda () (mu4e-message "Out of SAMPLE mu4e context"))
             :vars '(( user-mail-address . "foo@example.com")))))
  :custom
  (mail-user-agent 'mu4e-user-agent "Set mu4e a default MUA")
  (mu4e-compose-format-flowed t "Compose messages as format=flowed")
  (mu4e-sent-messages-behavior 'delete "Switch this behavior to 'sent within the appropriate contexts where directory mu4e-sent-folder is correctly set")
  (gnus-dired-mail-mode 'mu4e-user-agent)
  (mu4e-use-fancy-chars t "Use fancy unicode characters for mu4e marks")
  (mu4e-context-policy 'ask)
  (mu4e-compose-context-policy 'ask)
  (mu4e-index-update-in-background t "Index in background")
  (mu4e-mu-debug t "Run mu in debug mode")
  (mu4e-index-cleanup nil)
  (mu4e-index-lazy-check t)
  (mu4e-get-mail-command "true" "Noop during retrieval and just handle indexing")
  (mu4e-update-interval 300 "Auto index every 5 minutes"))

(use-package sendmail
  :straight (:type built-in)
  :custom
  (send-mail-function 'smtpmail-send-it "Default to block")
  (smtpmail-debug-info t "Enable debugging")
  (mail-specify-envelope-from nil "Don't try to be smart, use user-mail-address")
  (mail-envelope-from nil "Don't try to be smart, use user-mail-address"))

(use-package message
  :straight (:type built-in)
  :custom
  (message-directory "~/mail/")
  (message-send-mail-function 'message-send-mail-with-sendmail "Use sendmail as our MTA")
  (message-sendmail-f-is-evil t "Avoid setting -f (--from) when calling sendmail")
  (message-sendmail-envelope-from 'header "Use From: header")
  (message-kill-buffer-on-exit t "Kill a buffer once a message is sent"))

(message "💥 Debug on error is %s" debug-on-error)

(load "~/.emacs.d/lang.el")
(load "~/.emacs.d/personal.el")

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
