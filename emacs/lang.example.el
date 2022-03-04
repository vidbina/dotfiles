;; https://github.com/emacsmirror/rainbow-mode
(use-package rainbow-mode
  :straight (rainbow-mode :type git
                          :host github
                          :repo "emacsmirror/rainbow-mode"))

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
  (setq ispell-personal-dictionary "~/.hunspell_personal")

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Hooks-for-Loading.html
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-gnuplot.html#sec-4
(with-eval-after-load 'org
  (message "Loading org-babel-language mappings")
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (clojure .  t)
                                 (dot . t)
                                 (gnuplot . t)
                                 (haskell . t)
                                 (makefile . t)
                                 ;; (nix . t) ;; TODO: Figure out why broken
                                 (plantuml . t)
                                 (python . t))))

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

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :straight (dockerfile-mode :type git
                             :host github
                             :repo "spotify/dockerfile-mode"))

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
  :init
  (add-hook 'haskell-mode-hook 'haskell-unicode-input-method-enable))

;; https://github.com/jcollard/elm-mode
(use-package elm-mode
  :straight (elm-mode :type git
                          :host github
                          :repo "jcollard/elm-mode"))

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

;; https://github.com/emacsmirror/paredit
(use-package paredit
  :straight (paredit :type git
                     :host github
                     :repo "emacsmirror/paredit"))

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
