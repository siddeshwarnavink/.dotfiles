;; Config for Emacs v29

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
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file filename)))

(cond
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Monospace 14"))
 ((eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas 12")))

(load-theme 'wombat t)

(setq use-file-dialog nil)
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

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

(setq dired-dwim-target t)

(global-set-key "\M- " 'hippie-expand)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
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

(define-key viper-vi-global-user-map (kbd "C-e") 'move-end-of-line)
(define-key viper-vi-global-user-map (kbd "C-y") 'yank)
(define-key viper-vi-global-user-map (kbd "gd") 'lsp-find-definition)

(custom-set-faces '(viper-minibuffer-emacs ((t nil)))) ;; Get rid of ugly green overlay.

;; Ido
(ido-mode 1)
(ido-everywhere 1)

;; LSP
(when (memq system-type '(gnu/linux))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PATH")))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (((web-mode typescript-mode) . lsp-deferred))
  :commands lsp
  :custom
  (lsp-headerline-breadcrumb-enable nil))

(use-package company
  :bind (("C-y" . company-complete-selection))
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
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(use-package web-mode
  :config
  (setq web-mode-enable-auto-quoting nil))


(load-file custom-file)
