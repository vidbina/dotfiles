;; https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
(with-eval-after-load "ispell"
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "en_US,de_DE,nl")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ispell-dictionary)
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

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
  :init
  (setq plantuml-default-exec-mode 'executable))

;; https://github.com/ppareit/graphviz-dot-mode
(use-package graphviz-dot-mode
  :straight (graphviz-dot-mode :type git
                               :host github
                               :repo "ppareit/graphviz-dot-mode")
  :ensure t
  :config
  (setq graphviz-dot-indent-width 2))

;; https://github.com/emacsorphanage/gnuplot
;; also https://github.com/bruceravel/gnuplot-mode
;; also https://github.com/rudi/gnuplot-el
(use-package gnuplot
  :straight (gnuplot :type git
                     :host github
                     :repo "emacsorphanage/gnuplot"))

;; https://github.com/NixOS/nix-mode
(use-package nix-mode
  :straight (nix-mode :type git
                      :host github
                      :repo "NixOS/nix-mode")
  :init
  (setq nix-nixfmt-bin "nixpkgs-fmt"))

;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :straight (go-mode :type git
                     :host github
                     :repo "dominikh/go-mode.el"))

;; https://emacs-lsp.github.io/lsp-java/
(use-package lsp-java
  :straight (lsp-java :type git
                      :host github
                      :repo "emacs-lsp/lsp-java"))

;; https://github.com/emacsmirror/paredit
(use-package paredit
  :straight (paredit :type git
                     :host github
                     :repo "emacsmirror/paredit"))

;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :straight (clojure-mode :type git
                          :host github
                          :repo "clojure-emacs/clojure-mode")
  :config
  (require 'ob-clojure))

;; https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode
(use-package kotlin-mode
  :straight (kotlin-mode :type git
                         :host github
                         :repo "Emacs-Kotlin-Mode-Maintainers/kotlin-mode"))

;;;; https://github.com/clojure-emacs/cider
;;(use-package cider
;;  :straight (cider :type git
;;                   :host github
;;                   :repo "clojure-emacs/cider")
;;  :config
;;  (setq org-babel-clojure-backend 'cider
;;        cider-lein-parameters "with-profile -user repl :headless :host localhost"))

;; https://github.com/haskell/haskell-mode
(use-package haskell-mode
  :straight (haskell-mode :type git
                          :host github
                          :repo "haskell/haskell-mode")
  :init
  (add-hook 'haskell-mode-hook 'haskell-unicode-input-method-enable))

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
  :hook
  (clojure-mode . envrc-mode))

;; https://github.com/joaotavora/eglot
(use-package eglot
  :straight (eglot :type git
                   :host github
                   :repo "joaotavora/eglot"))

;; eldoc.el / eldoc-display-message-no-interference-p
;;   refers to
;; paren.el / show-paren--overlay and
;; paren.el / show-paren-context-when-offscreen
;;   which are bleeding edge

;; https://github.com/emacs-mirror/emacs/commit/9f505c476eb1a8e85ba26964abf218cab7db0e57

;; https://emacs-lsp.github.io/lsp-mode/page/installation/#vanilla-emacs
;;(use-package lsp-mode
;;  :straight (lsp-mode :type git
;;                      :host github
;;                      :repo "emacs-lsp/lsp-mode")
;;  :bind (:map lsp-mode-map ("TAB" . completion-at-point))
;;  :init
;;  (setq-default read-process-output-max (* 1024 1024))
;;  (setq  gc-cons-threshold (* 100 1024 1024))
;;  ;;(setq lsp-keymap-prefix "C-c C-M-l")
;;  ;;(setq lsp-log-io t) ;; ⚠️ turn off for performance
;;  :hook
;;  ;; (XXX-mode . lsp)
;;  (java-mode . lsp)
;;  (go-mode . lsp)
;;  :commands
;;  (lsp lsp-deferred)
;;  ;;:config
;;  ;;(define-key lsp-mode-map (kbd "C-c C-M-l") lsp-command-map)
;;  ;;(lsp-enable-which-key-integration t)
;;  )

;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package
;; https://config.daviwil.com/emacs
;; https://github.com/emacs-lsp/lsp-ui
;;(use-package lsp-ui
;;  ;;:straight t
;;  :hook (lsp-mode . lsp-ui-mode)
;;  :config
;;  (setq lsp-ui-sideline-enable t
;;        lsp-ui-sideline-show-hover nil
;;        lsp-ui-doc-position 'bottom)
;;  (lsp-ui-doc-show))

;;;; if you are ivy user
;;(use-package lsp-ivy
;;  :hook (lsp-mode . lsp-ivy-mode))

;;(use-package lsp-treemacs
;;  :commands lsp-treemacs-errors-list
;;  :after lsp-mode)

;;(setq lsp-docker-client-configs '((:server-id bash-ls :docker-server-id bashls-docker :server-command "bash-language-server start")
;;                                  (:server-id clangd :docker-server-id clangd-docker :server-command "clangd")
;;                                  (:server-id css-ls :docker-server-id cssls-docker :server-command "css-languageserver --stdio")
;;                                  (:server-id dockerfile-ls :docker-server-id dockerfilels-docker :server-command "docker-langserver --stdio")
;;                                  (:server-id gopls :docker-server-id gopls-docker :server-command "gopls")
;;                                  (:server-id html-ls :docker-server-id htmls-docker :server-command "html-languageserver --stdio")
;;                                  (:server-id pyls :docker-server-id pyls-docker :server-command "pyls")
;;                                  (:server-id ts-ls :docker-server-id tsls-docker :server-command "typescript-language-server --stdio")))

;;;; https://github.com/emacs-lsp/lsp-docker
;;(use-package lsp-docker
;;  :straight (lsp-docker :type git
;;                        :host github
;;                        :repo "emacs-lsp/lsp-docker")
;;  :after lsp-mode
;;  ;;:init
;;  ;;(setq lsp-docker-client-packages '()
;;  ;;      lsp-docker-client-configs '())
;;  :config
;;  (lsp-docker-init-clients :path-mappings '(("/home/vidbina/src" . "/projects"))
;;                           ;;:docker-image-id "vidbina/lsp-docker-langservers:latest"
;;                           :client-packages lsp-docker-default-client-packages
;;                           :client-configs lsp-docker-default-client-configs
;;                           ;;:client-configs '((:server-id 'clangd
;;                           ;;                              :docker-server-id 'examplels-docker
;;                           ;;                              :docker-image-id "vidbina/lsp-docker-langservers:latest"
;;                           ;;                              :docker-container-name "vidbina-lsp-clangd"
;;                           ;;                              :server-command "ccls"
;;                           ))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Hooks-for-Loading.html
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-gnuplot.html#sec-4
(with-eval-after-load 'org
  (message "Loading org-babel-language mappings")
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure .  t)
                                 (dot . t)
                                 (gnuplot . t)
                                 (haskell . t)
                                 (makefile . t)
                                 ;; (nix . t) ;; TODO: Figure out why broken
                                 (plantuml . t)
                                 (python . t))))
