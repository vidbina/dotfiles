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
                           :repo "gongo/json-reformat")
  :custom
  (json-reformat:indent-width 2 "Keep a short indentation span to simplify reading of deep structures"))

;; https://github.com/Sterlingg/json-snatcher
(use-package json-snatcher
  :straight (json-snatcher :type git
                           :host github
                           :repo "Sterlingg/json-snatcher"))

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
  :init
  (setq nix-nixfmt-bin "nixpkgs-fmt"))

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
  (define-key js-mode-map (kbd "C-c b") 'js-send-buffer))

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
  :hook (after-init . envrc-global-mode)
  :bind-keymap ("C-c e" . envrc-command-map))

;; https://github.com/joaotavora/eglot
(use-package eglot
  :straight (eglot :type git
                   :host github
                   :repo "joaotavora/eglot"))
