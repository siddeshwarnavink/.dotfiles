(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode)

;; Global settings
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-list-file-prefix emacs-tmp-dir
 auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
 backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq create-lockfiles nil)

(setq custom-file "~/.emacs.custom.el")

(set-face-attribute 'default nil :font "Monospace 14")

(load-theme 'wombat t)

(setq use-file-dialog nil)
(setq ring-bell-function 'ignore)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun my/setup-indent (n)
  (setq-default tab-width n)
  (setq c-basic-offset n)
  (setq js-indent-level n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n))
(my/setup-indent 4)

(global-set-key "\M- " 'hippie-expand)

(setq dired-dwim-target t)

(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(unless package--initialized (package-initialize))

(setq use-package-always-ensure t)
(require 'use-package)

;; Viper mode
(setq viper-mode t)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)
(setq viper-vi-style-in-minibuffer nil)
(setq viper-want-ctl-h-help 't)
(setq viper-ex-style-editing nil)
(setq viper-no-multiple-ESC 't)
(require 'viper)

(define-key viper-insert-global-user-map (kbd "C-]") 'viper-change-state-to-vi)
(define-key viper-vi-global-user-map (kbd "C-e") 'move-end-of-line)
(define-key viper-vi-global-user-map (kbd "C-y") 'yank)

;; Ido
(ido-mode 1)
(ido-everywhere 1)

;; LSP
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c <tab>" . company-complete)
              ("C-c e g n" . flymake-goto-next-error)
              ("C-c e g p" . flymake-goto-prev-error)
              ("C-c e g d" . eglot-find-implementation)
              ("C-c e r" . eglot-rename)
              ("C-c e f" . eglot-format-buffer)
              ("C-c e a" . eglot-code-actions))
  :hook (typescript-mode . eglot-ensure))

(use-package company
  :hook (eglot-managed-mode . company-mode)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 3))

;; Misc
(add-hook 'prog-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode))

;; Major modes
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'"))

(load-file custom-file)
