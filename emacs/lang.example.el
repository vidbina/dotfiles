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
  (ispell-dictionary "en_US,de_DE,nl_NL,fr-toutesvariantes,es_ANY")
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

;; https://github.com/abrochard/mermaid-mode
(use-package mermaid-mode
  :straight (mermaid-mode :type git
                          :host github
                          :repo "abrochard/mermaid-mode"))

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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

(defun vidbina/init-eglot-pyright-config ()
  "Stub a pyrightconfig.json file for a project"
  (interactive)

  (let* ((project-dir default-directory)
         (venv-dir ".venv"))

    (message (format "🐍 Writing config for %s with venv in %s" project-dir venv-dir))
    (with-temp-buffer
      (insert (json-serialize `((venvPath . ,project-dir)
                                (venv . ,venv-dir)
                                (verboseOutput . :false)
                                (typeCheckingMode . "strict")
                                (useLibraryCodeForTypes . t)
                                (defineConstant . ((DEBUG . t))))))
      (json-pretty-print-buffer)
      (append-to-file (point-min) (point-max) (expand-file-name "pyrightconfig.json" project-dir)))))

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

;; https://github.com/gleam-lang/gleam-mode
(use-package gleam-ts-mode
  :straight (gleam-ts-mode :type git
                           :host github
                           :repo "gleam-lang/gleam-mode"))

;; https://github.com/immerrr/lua-mode
(use-package lua-mode
  :straight (lua-mode :type git
                      :host github
                      :repo "immerrr/lua-mode"))

;; https://github.com/mcandre/vimrc-mode
(use-package vimrc-mode
  :straight (vimrc-mode :type git
                      :host github
                      :repo "mcandre/vimrc-mode"))

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

;; https://github.com/emacsorphanage/applescript-mode
(use-package applescript-mode
  :straight (applescript-mode :type git
                              :host github
                              :repo "emacsorphanage/applescript-mode"))

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

(use-package treesit
  :straight (:type built-in)
  :init
  (setq treesit-language-source-alist
        '(
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (gleam . ("https://github.com/gleam-lang/tree-sitter-gleam"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
          (elm . ("https://github.com/elm-tooling/tree-sitter-elm"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          ))
  ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  :config
  (setq major-mode-remap-alist
        '(
          (json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (python-mode . python-ts-mode)
          (gleam-mode . gleam-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (css-mode . css-ts-mode)
          ))
  (add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . tsx-ts-mode))
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	(treesit-install-language-grammar lang)
	(message "`%s' parser was installed." lang)
	(sit-for 0.75)))))

;; https://github.com/emacs-tree-sitter/tree-sitter-langs
(use-package tree-sitter-langs
  :straight (tree-sitter-langs :type git
                               :host github
                               :repo "emacs-tree-sitter/tree-sitter-langs")
  :after tree-sitter)

;; https://github.com/mickeynp/combobulate
(use-package combobulate
  :straight (combobulate :type git
                         :host github
                         :repo "mickeynp/combobulate"))

;; https://github.com/radian-software/apheleia
(use-package apheleia
  :straight (apheleia :type git
                         :host github
                         :repo "radian-software/apheleia")
  :ensure t
  :config
  (apheleia-global-mode +1)
  :delight
  (apheleia-mode "👨🏿‍🏭"))

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

;; https://github.com/karthink/gptel
(use-package gptel
  :straight (gptel :type git
                   :host github
                   :repo "karthink/gptel")
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-api-key (lambda ()
                        (auth-source-pass-get 'secret "openai.com/david@asabina.de/api-key-2023.04.18-emacs-vidbina")))

  :custom
  (gptel-directives
   '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     (chat . "You are a large language model and a conversation partner. Respond concisely.")
     (vid . "You are a technical analyst with a strong background in EE an CS. Respond concisely and assume that the reader has the same background which warrants the avoidance of explanation of technical concepts unless explicitly asked for.")
     ))
  )

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el"))
  :ensure t
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "M-p") 'copilot-previous-completion))

(with-eval-after-load 'flymake
  ;; Set flymake bindings
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (defun vidbina/jump-to-active-line-in-consult-flymake ()
    "Jump to the current line in consult-flymake"
    (let* ((target-line (line-number-at-pos))
           (timer (run-at-time 1 nil
                               `(lambda ()
                                  ;; Stubbing cancel hook
                                  (defun vidbina/jump-to-active-line-in-consult-flymake--cancel ()
                                    (message "🪂 Cancelling timer")
                                    (advice-remove 'vertico-exit #'vidbina/jump-to-active-line-in-consult-flymake)
                                    (advice-remove 'exit-minibuffers #'vidbina/jump-to-active-line-in-consult-flymake))

                                  (message "🪂 Arming timer cancellation on minibuffer escape")
                                  ;; Arm (abort-minibuffers) and (exit-minibuffers) called by vertico-exit to cancel jump helper
                                  (advice-add 'abort-minibuffers :before #'vidbina/jump-to-active-line-in-consult-flymake--cancel)
                                  (advice-add 'exit-minibuffers :before #'vidbina/jump-to-active-line-in-consult-flymake--cancel)
                                  (message "🪂 Executing jump to %s in buffer %s" ,(number-to-string target-line) (buffer-name))
                                  ;; Note that entering the digits is not enough to update the position in vertico
                                  (mapcar (lambda (x) (self-insert-command 1 x)) ,(number-to-string target-line))
                                  (insert "")))))
      (message "🪂 Armed jumper to %s" (number-to-string target-line))))

  (advice-add 'consult-flymake :before #'vidbina/jump-to-active-line-in-consult-flymake))

;; https://github.com/joaotavora/eglot
(use-package eglot
  :straight (eglot :type git
                   :host github
                   :repo "joaotavora/eglot")
  ;; :hook
  ;; (eglot-managed-mode-hook . (lambda ()
  ;;                              ;; Show flymake diagnostics first.
  ;;                              (setq eldoc-documentation-functions
  ;;                                    (cons #'flymake-eldoc-function
  ;;                                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
  ;;                              ;; Show all eldoc feedback.
  ;;                              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))
  :custom
  (eglot-autoshutdown t)

  :bind (("C-c j" . eglot)
         :map eglot-mode-map
         ("C-c j f d" . eglot-find-declaration)
         ("C-c j f i" . eglot-find-implementation)
         ("C-c j f t" . eglot-find-typeDefinition)
         ("C-c j j j" . eglot)
         ("C-c j j r" . eglot-reconnect)
         ("C-c j h s" . eglot-signature-eldoc-function)
         ("C-c j h h" . eglot-hover-eldoc-function)
         ("C-c j \\" . eglot-format)
         ("C-c j k" . eglot-shutdown)
         ("C-c j j k" . eglot-rename)))
