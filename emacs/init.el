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
  :after htmlize
  :init
  (setq org-adapt-indentation nil ; https://orgmode.org/manual/Hard-indentation.html
        org-hide-leading-stars nil
        org-odd-levels-only nil)
  :hook
  (org-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c d") 'org-hide-drawer-toggle)
  ;; https://www.reddit.com/r/emacs/comments/ldiryk/weird_tab_behavior_in_org_mode_source_blocks
  (setq org-src-preserve-indentation t
        org-hide-block-startup t)

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
  (org-src-window-setup 'current-window "Show edit buffer in calling window"))

;; https://github.com/jkitchin/ox-clip
;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
(use-package ox-clip
  :straight (ox-clip :type git
                     :host github
                     :repo "jkitchin/ox-clip")
  :after (org htmlize)
  :bind
  ("C-c y" . ox-clip-formatted-copy))

(use-package htmlize
  :straight (htmlize :type git
                     :host github
                     :branch "fix-face-size-unspecified-head"
                     :repo "vidbina/emacs-htmlize"
                     :local-repo "~/src/vidbina/emacs-htmlize")
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
  :after org-roam)

;; https://github.com/alphapapa/org-ql
(use-package org-ql
  :straight (org-ql :type git
                    :host github
                    :repo "alphapapa/org-ql"))

;; https://github.com/astahlman/ob-async
(use-package ob-async
  :straight (ob-async :type git
                      :host github
                      :repo "astahlman/ob-async"))

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

;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :after
  (:all delight diminish)
  :straight
  (smart-mode-line :type git
                   :host github
                   :repo "Malabarba/smart-mode-line"))

;; https://git.savannah.nongnu.org/git/delight.git
(use-package delight
  :straight (delight :type git
                     :host nil
                     :repo "https://git.savannah.nongnu.org/git/delight.git")
  :delight
  (auto-revert-mode "♻️")
  (eldoc-mode "el📖")
  (edebug-mode "🐞")
  (global-whitespace-mode)
  (visual-line-mode "🌯")
  (mu4e-main-mode "📫")
  (mu4e-headers-mode "📬")
  (mu4e-view-mode "📧"))

;; https://github.com/myrjola/diminish.el
(use-package diminish
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
  :hook ((after-init . default-text-scale-mode)))

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
  (with-eval-after-load 'persp-mode
    (message "Configuring zoom-window to work with persp-mode")
    (setq zoom-window-use-persp t)))

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
  (async-bytecomp-package-mode 1))

(defvar ob-async-inject-variables-exclude "-overlays"
  "Regex of variables to exclude from injection into the async process.
It's a good idea to exclude any variables that contain overlays.")

;;;###autoload
(defun ob-async-org-babel-execute-src-block (&optional orig-fun arg info params)
  "Like org-babel-execute-src-block, but run asynchronously.

Original docstring for org-babel-execute-src-block:

Execute the current source code block.  Insert the results of
execution into the buffer.  Source code execution and the
collection and formatting of results can be controlled through a
variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive "P")
  (cond
   ;; If this function is not called as advice, do nothing
   ((not orig-fun)
    (warn "ob-async-org-babel-execute-src-block is no longer needed in org-ctrl-c-ctrl-c-hook")
    nil)
   ;; If there is no :async parameter, call the original function
   ((not (assoc :async (nth 2 (or info (org-babel-get-src-block-info)))))
    (funcall orig-fun arg info params))
   ;; If the src block language is in the list of languages async is not to be
   ;; used for, call the original function
   ((member (nth 0 (or info (org-babel-get-src-block-info)))
            ob-async-no-async-languages-alist)
    (funcall orig-fun arg info params))
   ;; Otherwise, perform asynchronous execution
   (t
    (let ((placeholder (ob-async--generate-uuid)))
      ;; Here begins the original source of org-babel-execute-src-block
      (let* ((org-babel-current-src-block-location
              (or org-babel-current-src-block-location
                  (nth 5 info)
                  (org-babel-where-is-src-block-head)))
             (src-block-marker (save-excursion
                                 (goto-char org-babel-current-src-block-location)
                                 (point-marker)))
             (info (if info (copy-tree info) (org-babel-get-src-block-info))))
        ;; Merge PARAMS with INFO before considering source block
        ;; evaluation since both could disagree.
        (cl-callf org-babel-merge-params (nth 2 info) params)
        (when (org-babel-check-evaluate info)
          (cl-callf org-babel-process-params (nth 2 info))
          (let* ((params (nth 2 info))
                 (cache (let ((c (cdr (assq :cache params))))
                          (and (not arg) c (string= "yes" c))))
                 (new-hash (and cache (org-babel-sha1-hash info)))
                 (old-hash (and cache (org-babel-current-result-hash)))
                 (current-cache (and new-hash (equal new-hash old-hash)))
                 (result-params (cdr (assq :result-params params))))
            (cond
             (current-cache
              (save-excursion		;Return cached result.
                (goto-char (org-babel-where-is-src-block-result nil info))
                (forward-line)
                (skip-chars-forward " \t")
                (let ((result (org-babel-read-result)))
                  (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
                  result)))
             ((org-babel-confirm-evaluate info)
              ;; Insert a GUID as a placeholder in our RESULTS block
              (when (not (or (member "none" result-params)
                             (member "silent" result-params)))
                (org-babel-insert-result placeholder '("replace")))
              (let* ((lang (nth 0 info))
                     ;; Expand noweb references in BODY and remove any
                     ;; coderef.
                     (body
                      (let ((coderef (nth 6 info))
                            (expand
                             (if (org-babel-noweb-p params :eval)
                                 (org-babel-expand-noweb-references info)
                               (nth 1 info))))
                        (if (not coderef) expand
                          (replace-regexp-in-string
                           (org-src-coderef-regexp coderef) "" expand nil nil 1))))
                     (dir (cdr (assq :dir params)))
                     (default-directory
                       (or (and dir (file-name-as-directory (expand-file-name dir)))
                           default-directory))
                     (cmd (intern (concat "org-babel-execute:" lang)))
                     (org-babel-async-content
                      (buffer-substring-no-properties (point-min) (point-max)))
                     result)
                (unless (fboundp cmd)
                  (error "No org-babel-execute function for %s!" lang))
                (message "executing %s code block%s..."
                         (capitalize lang)
                         (let ((name (nth 4 info)))
                           (if name (format " (%s)" name) "")))
                (progn
                  (async-start
                   `(lambda ()
                      ;; TODO: Put this in a function so it can be overidden
                      ;; Initialize the new Emacs process with org-babel functions
                      (setq exec-path ',exec-path)
                      (setq load-path ',load-path)
                      ,(async-inject-variables ob-async-inject-variables nil ob-async-inject-variables-exclude)
                      (package-initialize)
                      (setq ob-async-pre-execute-src-block-hook ',ob-async-pre-execute-src-block-hook)
                      (run-hooks 'ob-async-pre-execute-src-block-hook)
                      (org-babel-do-load-languages 'org-babel-load-languages ',org-babel-load-languages)
                      (let ((default-directory ,default-directory))
                        (with-temp-buffer
                          (insert org-babel-async-content)
                          (,cmd ,body ',params))))
                   `(lambda (result)
                      (with-current-buffer ,(current-buffer)
                        (let ((default-directory ,default-directory))
                          (save-excursion
                            (cond
                              ((member "none" ',result-params)
                               (message "result silenced"))
                              ((member "silent" ',result-params)
                               (message (replace-regexp-in-string "%" "%%" (format "%S" result))))
                              (t
                               (goto-char ,src-block-marker)
                               (let ((file (cdr (assq :file ',params))))
                                 (when file
                                   ;; when result type is link, don't write result content to file.
                                   (unless (member "link" ',result-params)
                                     ;; If non-empty result and :file then write to :file.
                                     (when result
                                       (with-temp-file file
                                         (insert (org-babel-format-result
                                                  result (cdr (assq :sep ',params)))))))
                                   (setq result file))
                                 ;; Possibly perform post process provided its
                                 ;; appropriate.  Dynamically bind "*this*" to the
                                 ;; actual results of the block.
                                 (let ((post (cdr (assq :post ',params))))
                                   (when post
                                     (let ((*this* (if (not file) result
                                                     (org-babel-result-to-file
                                                      file
                                                      (let ((desc (assq :file-desc ',params)))
                                                        (and desc (or (cdr desc) result)))))))
                                       (setq result (org-babel-ref-resolve post))
                                       (when file
                                         (setq result-params (remove "file" ',result-params))))))
                                 (org-babel-insert-result result ',result-params ',info ',new-hash ',lang))))
                            (run-hooks 'org-babel-after-execute-hook)))))))))))))))))

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
                   :repo "magit/magit"))

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :straight (diff-hl :type git
                     :host github
                     :repo "dgutov/diff-hl")
  :hook (after-init . global-diff-hl-mode))

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

;; https://www.djcbsoftware.nl/code/mu/mu4e.html
(use-package mu4e
  :straight (:type built-in)
  :demand t
  :bind (("C-c M 4" . mu4e))
  :hook (
         ;; https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html
         (dired-mode . turn-on-gnus-dired-mode))
  :config
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-compose-format-flowed t
        mu4e-contexts `( ,(make-mu4e-context
                           :name "Sample"
                           :enter-func (lambda () (mu4e-message "Into SAMPLE mu4e context"))
                           :leave-func (lambda () (mu4e-message "Out of SAMPLE mu4e context"))
                           :vars '(( user-mail-address . "foo@example.com"))))
        mu4e-context-policy 'always-ask
        mu4e-index-update-in-background t
        mu4e-view-show-addresses t)

  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Retrieval-and-indexing.html#Speeding-up-indexing
  (setq mu4e-index-cleanup nil      ; don't do a full cleanup check
        mu4e-index-lazy-check t)    ; don't consider up-to-date dirs

  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Retrieval-and-indexing.html#Example-setup
  (setq mu4e-get-mail-command "offlineimap"   ; or fetchmail, or ...
        mu4e-update-interval 300)             ; update every 5 minutes

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

  (setq gnus-dired-mail-mode 'mu4e-user-agent))

;; https://git.notmuchmail.org/git/notmuch
(use-package notmuch
  :straight (:type built-in)
  :if (executable-find "notmuch")
  :commands (notmuch
             notmuch-tree
             notmuch-search
             notmuch-hello)
  :bind (("C-c M n" . notmuch)
         :map notmuch-search-mode-map
         ("SPC" . vidbina/notmuch-toggle-inbox))
  :init
  (evil-collection-notmuch-setup)
  :hook (notmuch-hello-mode . (lambda () (display-line-numbers-mode 0)))
  :custom
  (mail-envelope-from 'header)
  (mail-specify-envelope-from t)
  (message-kill-buffer-on-exit t)
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (message-sendmail-envelope-from 'header)
  (message-sendmail-f-is-evil nil)
  (message-signature #'my/mail-sig)
  (notmuch-always-prompt-for-sender t)
  (notmuch-archive-tags '("-inbox" "-unread"))
  (notmuch-crypto-process-mime t)
  (notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (notmuch-labeler-hide-known-labels t)
  (notmuch-message-headers '("Subject" "To" "Cc" "Bcc"))
  (notmuch-search-oldest-first nil)
  (sendmail-program (executable-find "msmtp"))
  :config
  (notmuch-address-harvest)
  (require 'ol-notmuch))

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
  (persp-mode t))

(message "💥 Debug on error is %s" debug-on-error)

(load "~/.emacs.d/lang.el")
(load "~/.emacs.d/personal.el")

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
