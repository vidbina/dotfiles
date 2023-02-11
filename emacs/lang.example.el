;; Tangled from dotfiles/emacs/README.org

;; https://github.com/emacsmirror/rainbow-mode
(use-package rainbow-mode
  :straight (rainbow-mode :type git
                          :host github
                          :repo "emacsmirror/rainbow-mode"))

;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
(use-package ispell
  :straight (:type built-in)
  :custom
  (ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (ispell-dictionary "en_US,de_DE,nl")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (ispell-personal-dictionary "~/.hunspell_personal")
  (ispell-personal-dictionary "~/.hunspell_personal")
  :config
  ;; https://www.emacswiki.org/emacs/FlySpell#h5o-14
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :straight (markdown-mode :type git
                           :host github
                           :repo "jrblevin/markdown-mode")
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :straight (json-mode :type git
                       :host github
                       :repo "joshwnj/json-mode"))

;; https://github.com/gongo/json-reformat
(use-package json-reformat
  :straight (json-reformat :type git
                           :host github
                           :repo "gongo/json-reformat"))

;; https://github.com/Sterlingg/json-snatcher
(use-package json-snatcher
  :straight (json-snatcher :type git
                           :host github
                           :repo "Sterlingg/json-snatcher"))

;; https://github.com/tminor/jsonnet-mode
(use-package jsonnet-mode
  :straight (jsonnet-mode :type git
                          :host github
                          :repo "tminor/jsonnet-mode"))

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :straight (yaml-mode :type git
                       :host github
                       :repo "yoshiki/yaml-mode"))

;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :straight (plantuml-mode :type git
                           :host github
                           :repo "skuro/plantuml-mode")
  :after org
  :config
  ;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-dot.html
  (setq org-plantuml-exec-mode 'plantuml)

  (setq plantuml-default-exec-mode 'executable)
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((plantuml . t)))))

;; https://github.com/ppareit/graphviz-dot-mode
(use-package graphviz-dot-mode
  :straight (graphviz-dot-mode :type git
                               :host github
                               :repo "ppareit/graphviz-dot-mode")
  :after org
  :config
  (setq graphviz-dot-indent-width 2)
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((dot . t)))))

;; https://github.com/emacsorphanage/gnuplot
;; also https://github.com/bruceravel/gnuplot-mode
;; also https://github.com/rudi/gnuplot-el
(use-package gnuplot
  :straight (gnuplot :type git
                     :host github
                     :repo "emacsorphanage/gnuplot")
  :after org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((gnuplot . t)))))

(with-eval-after-load 'org
  (message "Load Shell into Org Babel")
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((shell . t)))))

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :straight (dockerfile-mode :type git
                             :host github
                             :repo "spotify/dockerfile-mode"))

(with-eval-after-load 'org
  (message "Load Octave into Org Babel")
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((octave . t)))))

;; https://github.com/NixOS/nix-mode
(use-package nix-mode
  :straight (nix-mode :type git
                      :host github
                      :repo "NixOS/nix-mode")

  :custom
  (nix-nixfmt-bin "nixpkgs-fmt"))

(defcustom nix-develop-default-prompt-regexp "^>\s+"
  "Custom prompt for nix-develop"
  :type 'string
  :group 'nix-develop)

(defvar nix-develop-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defvar nix-develop-mode-syntax-table
  (make-syntax-table shell-mode-syntax-table))

(define-derived-mode nix-develop-mode comint-mode "Nix Develop"
  "Major mode for `nix-develop'"
  (setq comint-prompt-regexp nix-develop-default-prompt-regexp))

(defun org-babel-execute:nix-develop (body params)
  "Execute a block of nix develop commands with Babel."
  (save-window-excursion
    (let* ((shell-buffer (org-babel-sh-initiate-session "*nix-develop*"))
           (prompt-regexp nix-develop-default-prompt-regexp))
      (org-babel-comint-with-output
          (shell-buffer org-babel-sh-eoe-output t body)
        (dolist (line (append (list "nix develop")
                              (split-string (org-trim body) "\n")
                              (list org-babel-sh-eoe-indicator)))
          (insert line)
          (comint-send-input nil t)
          (while (save-excursion
                   (goto-char comint-last-input-end)
                   (not (re-search-forward
                         prompt-regexp nil t)))
            (accept-process-output
             (get-buffer-process (current-buffer)))))))))

(provide 'nix-develop-mode)

;; https://github.com/fxbois/web-mode
(use-package web-mode
  :straight (web-mode :type git
                      :host github
                      :repo "fxbois/web-mode"))

;; https://github.com/fxbois/web-mode
(use-package web-mode
  :straight (web-mode :type git
                      :host github
                      :repo "fxbois/web-mode"))

;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :straight (go-mode :type git
                     :host github
                     :repo "dominikh/go-mode.el"))

;; https://github.com/redguardtoo/js-comint
(use-package js-comint
  :straight (js-comint :type git
                       :host github
                       :repo "redguardtoo/js-comint")
  :hook (inferior-js-mode . (lambda ()
                              (add-hook 'comint-output-filter-functions 'js-comint-process-output)))
  :config
  (define-key js-mode-map [remap eval-last-sexp] #'js-comint-send-last-sexp)
  (define-key js-mode-map (kbd "C-c b") 'js-send-buffer)
  :custom
  (js-indent-level 2))

;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :straight
  (typescript-mode :type git
                   :host github
                   :repo "emacs-typescript/typescript.el")
  :delight
  (typescript-mode "ts")
  :custom
  (typescript-indent-level 2))

;; https://github.com/ananthakumaran/tide
(use-package tide
  :straight
  (tide :type git
        :host github
        :repo "ananthakumaran/tide")

  :init
  (evil-collection-tide-setup)
  :delight
  (tide-mode "🌊")
  :after (typescript-mode evil-collection)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :custom
  (tide-always-show-documentation nil "Don't show docs if only type info is available to minimize disruption"))

;; https://emacs-lsp.github.io/lsp-java/
(use-package lsp-java
  :straight (lsp-java :type git
                      :host github
                      :repo "emacs-lsp/lsp-java"))

;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :straight (clojure-mode :type git
                          :host github
                          :repo "clojure-emacs/clojure-mode")
  :config
  (require 'ob-clojure))

;; https://github.com/clojure-emacs/cider
(use-package cider
  :straight (cider :type git
                   :host github
                   :repo "clojure-emacs/cider")
  :config
  (setq org-babel-clojure-backend 'cider
        cider-lein-parameters "with-profile -user repl :headless :host localhost"))

;; https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode
(use-package kotlin-mode
  :straight (kotlin-mode :type git
                         :host github
                         :repo "Emacs-Kotlin-Mode-Maintainers/kotlin-mode"))

;; https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  :straight (swift-mode :type git
                        :host github
                        :repo "swift-emacs/swift-mode"))

;; https://github.com/haskell/haskell-mode
(use-package haskell-mode
  :straight (haskell-mode :type git
                          :host github
                          :repo "haskell/haskell-mode")
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  :hook ((haskell-mode . haskell-unicode-input-method-enable)
         (haskell-mode . interactive-haskell-mode))
  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-stylish-on-save t))

;; https://github.com/jcollard/elm-mode
(use-package elm-mode
  :straight (elm-mode :type git
                      :host github
                      :repo "jcollard/elm-mode"))

;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :straight (rust-mode :type git
                       :host github
                       :repo "rust-lang/rust-mode"))

;; https://github.com/editorconfig/editorconfig-emacs#readme
(use-package editorconfig
  :straight (editorconfig :type git
                          :host github
                          :repo "editorconfig/editorconfig-emacs")
  :config
  (editorconfig-mode 1)
  :delight
  (editorconfig-mode "🎛️"))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight (rainbow-delimiters :type git
                                :host github
                                :repo "Fanael/rainbow-delimiters")
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (prog-mode . rainbow-delimiters-mode))

  ;; ;; https://github.com/patrickt/emacs
  ;; ((prog-mode) . rainbow-delimiters-mode)
  )

;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :straight (highlight-indent-guides :type git
                                     :host github
                                     :repo "DarthFennec/highlight-indent-guides")
  :custom
  (highlight-indent-guides-method 'column))

;; https://github.com/emacsmirror/paredit
(use-package paredit
  :straight (paredit :type git
                     :host github
                     :repo "emacsmirror/paredit")
  :delight
  (paredit-mode "🛝")
  :bind (("C-c v (" . paredit-mode)))

;; https://github.com/purcell/inheritenv
(use-package inheritenv
  :straight (inheritenv :type git
                        :host github
                        :repo "purcell/inheritenv"))

;; https://github.com/purcell/envrc
(use-package envrc
  :straight (envrc :type git
                   :host github
                   :repo "purcell/envrc")
  :after inheritenv
  :delight
  (envrc-mode "📦")
  :hook (after-init . envrc-global-mode)
  :bind-keymap ("C-c e" . envrc-command-map))

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :type git
                                  :host github
                                  :repo "purcell/exec-path-from-shell")
  :config (when (daemonp)
            (exec-path-from-shell-initialize)))

;; https://github.com/junjizhi/aide.el
(use-package aide
  :straight (aide :type git
                  :host github
                  :repo "vidbina/aide.el"
                  :branch "vidbina/retrieve-secret-through-function")
  :custom
  (aide-completions-model "text-davinci-003")
  (aide-openai-api-key-getter (lambda ()
                                (auth-source-pass-get 'secret "openai.com/david@asabina.de/api-key-2022.02.18"))))

;; https://github.com/joaotavora/eglot
(use-package eglot
  :straight (eglot :type git
                   :host github
                   :repo "joaotavora/eglot")
  :bind (:map eglot-mode-map ("C-h j" . xref-find-definition)))
