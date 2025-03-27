;;
;; Copyright (c) 2024 - Siddeshwar's Emacs experience.
;;
;;
;; Config written for GNU Emacs v29.3.
;; It probably won't work if you're not Siddeshwar.

(setq user-full-name "Siddeshwar"
      user-mail-address "siddeshwar.work@gmail.com")

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(tooltip-mode 0)

;; Enable disabled functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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
  (with-temp-buffer (write-file custom-file)))

(setq kill-ring-max 1000)
(defun sid-set-font (frame)
  "Set the default font for the specified FRAME."
  (select-frame frame)
  (if (eq system-type 'gnu/linux)
      (set-face-attribute 'default frame :font "Monospace 14")
    (if (eq system-type 'windows-nt)
        (set-face-attribute 'default frame :font "Consolas 12")
      (if (eq system-type 'darwin)
          (set-face-attribute 'default frame :font "Menlo 12")))))
(sid-set-font (selected-frame))
(add-hook 'after-make-frame-functions 'sid-set-font)

;; UI
(load-theme 'modus-operandi t)

(setq use-file-dialog nil)
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun sid-setup-indent (n)
  (setq-default tab-width n)
  (setq c-basic-offset n)
  (setq js-indent-level n)
  (setq css-indent-offset n))
(sid-setup-indent 4)

(setq dired-dwim-target 't)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; Keybinding
(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c o") #'mode-line-other-buffer)

(defun sid-kill-backward ()
  "Kill text from the cursor to the beginning of the line."
  (interactive)
  (kill-region (point) (line-beginning-position)))
(global-set-key (kbd "C-S-k") 'sid-kill-backward)

(defun sid-kill-current-buffer()
  "Kill the current buffer without confirmation."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer (current-buffer))))
(global-set-key (kbd "C-c k") 'sid-kill-current-buffer)

(defun sid-join-line()
  "Concatinate current line with next line."
  (interactive)
  (next-line)
  (delete-indentation))
(global-set-key (kbd "C-c j") 'sid-join-line)

(defun sid-open-dired ()
  "Open Dired for the directory of the current buffer's file."
  (interactive)
  (if buffer-file-name
      (dired (file-name-directory buffer-file-name))
    (message "Buffer is not visiting a file")))
(global-set-key (kbd "C-c p") 'sid-open-dired)

(defun sid-html-preview ()
  "View the current buffer's HTML content in eww."
  (interactive)
  (let ((temp-file "/tmp/temp.html"))
    (write-region (point-min) (point-max) temp-file)
    (eww-open-file temp-file)))
(global-set-key (kbd "C-c h") 'sid-html-preview)

(defun sid-clear-and-reexecute()
  "Clear and re-execute the inferior shell."
  (interactive)
  (comint-clear-buffer)
  (comint-previous-input 0)
  (comint-send-input))
(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "C-c r") 'sid-clear-and-reexecute))

(defun sid-smerge-keep-lower-all ()
  "Run `smerge-keep-lower` for all diffs in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^<<<<<<< " nil t)
      (smerge-keep-lower))))

(defun sid-smerge-keep-upper-all ()
  "Run `smerge-keep-upper` for all diffs in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^<<<<<<< " nil t)
      (smerge-keep-upper))))

(defun sid-smerge-mode-setup ()
  (local-set-key (kbd "C-c s l") 'sid-smerge-keep-lower-all)
  (local-set-key (kbd "C-c s u") 'sid-smerge-keep-upper-all))
(add-hook 'smerge-mode-hook 'sid-smerge-mode-setup)

(defun sid-git-rebase-accept-all-incomming (branch)
  "Accept all incomming changes for all conflict."
  (interactive "sRebase onto? ")
  (async-shell-command (format "git rebase --strategy-option=theirs %s" branch)))

(defun sid-save-file-path ()
  "Save the current file path to the kill ring if the buffer is visiting a file."
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new (file-truename buffer-file-name))
        (message "Saved file path to kill ring: %s" (file-truename buffer-file-name)))
    (message "Current buffer is not visiting a file")))
(global-set-key (kbd "C-c w") 'sid-save-file-path)

(defun sid-yank-relative-path ()
  "Yank the relative path from the current buffer"
  (interactive)
  (let ((latest-kill (current-kill 0 t)))
    (if (and buffer-file-name (file-exists-p latest-kill))
        (progn
          (insert (file-relative-name latest-kill (file-name-directory buffer-file-name)))
          (message "Inserted relative path: %s" (file-relative-name latest-kill (file-name-directory buffer-file-name))))
      (yank)
      (message "Yanked from kill ring"))))
(global-set-key (kbd "C-c y") 'sid-yank-relative-path)

;; Macros
(defalias 'html-li
  (kmacro "< l i > C-e < / l i > C-n C-a"))

(defalias 'cardio
  (kmacro "C-x C-x C-SPC <return> C-p / * C-n C-a C-SPC C-s * <return> C-w C-e <backspace> <backspace> : C-n M-m C-d C-M-f C-f C-d C-d C-e <return> * C-n C-d C-e <backspace> <backspace> C-n M-m C-x SPC M-} C-x r t * <return> / C-SPC M-< C-M-\\"))

(defalias 'file-accept-all-lower-smerge\
   (kmacro "<return> C-c s l C-x C-s C-c k"))

;; Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(unless package--initialized (package-initialize))

(setq use-package-always-ensure t)
(require 'use-package)

(when (memq system-type '(gnu/linux))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PATH")))

;; Minibuffer
(savehist-mode 1)
(recentf-mode 1)

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-count 6))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

;; Editing
(delete-selection-mode 1)
(global-subword-mode 1)

(use-package multiple-cursors
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode))

;; Navigating
(use-package dumb-jump
  :bind (("M-g j" . dumb-jump-go))
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package consult
  :defer t
  :bind (("C-x b" . consult-buffer)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-y"   . consult-yank-pop))
  :init (setq consult-preview-key 'any))

;; Misc
(use-package which-key
  :init
  (which-key-mode))

(use-package magit
  :defer t
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(add-hook 'prog-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

;; Language specific modes
(use-package tree-sitter
  :defer t
  :hook ((typescript-ts-mode . tree-sitter-mode)
         (typescript-ts-mode . tree-sitter-hl-mode)
         (tsx-ts-mode . tree-sitter-mode)
         (tsx-ts-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :defer t
  :after tree-sitter)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package clojure-mode
  :defer t
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'"))

(use-package cider
  :defer t
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-repl-display-help-banner nil))

;; Org mode
(setq
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

(defun sid-org-mode()
  (local-set-key (kbd "C-c m") 'org-modern-mode)
  (setq line-spacing 0.33))
(add-hook 'org-mode-hook 'sid-org-mode)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'org)
  (require 'ob)
  (add-hook 'org-mode-hook (lambda () (require 'org-tempo))))

(use-package org-modern
  :commands (org-modern-mode))

(global-set-key (kbd "C-c a a") 'org-agenda)
(setq org-agenda-files (list (expand-file-name "~/org/tasks.org")))
(setq org-log-done 'time)

(load-file custom-file)
