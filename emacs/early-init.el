(message "🥱 Loading early-init.el")

(setq package-enable-at-startup nil)

(setq use-package-verbose t)

(add-hook 'before-init-hook (lambda () (message "🪝 Before init")))
(add-hook 'after-init-hook (lambda () (message "🪝 After init")))
(add-hook 'emacs-startup-hook (lambda () (message "🪝 Emacs startup")))
(add-hook 'window-setup-hook (lambda () (message "🪝 Window setup")))
