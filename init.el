;;; init.el --- Rawley Fowler's Emacs Configuration
;;; Commentary:
;;; A simple Emacs configuration with Evil and Ivy, for Scala and Perl development.

;;; Code:
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Monaco-16")
  (add-to-list 'default-frame-alist '(font . "Hurmit Nerd Font-16")))
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
(setq package-enable-at-startup nil)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-ic")
(setq indent-tabs-mode nil)
(setq require-final-newline t)
(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)
(setq show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(recentf-mode 1)
(global-display-line-numbers-mode 1)
(add-to-list 'image-types 'svg)
(setq require-final-newline t)

(defun set-exec-path-from-shell ()
  "Set up Emacs' \='exec-path' and PATH environment."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell)

(add-to-list 'load-path "~/.emacs.d/extras")

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'control))

(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(global-set-key (kbd "M-o") #'other-window)

(require 'package)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package raku-mode)
(use-package web-mode)
(use-package json-mode)
(use-package dockerfile-mode)

(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :hook
  (scala-mode . lsp)
  (java-mode . lsp)
  (ruby-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :init
  (setq lsp-completion-enable t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-keep-workspace-alive nil))

(use-package lsp-metals)
(use-package lsp-ui)
(use-package yasnippet)

(use-package smartparens
  :diminish smartparens-mode
  :ensure smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package lsp-java
  :after lsp)

(use-package java
  :ensure nil
  :after lsp-java
  :bind (:map java-mode-map ("C-c i" . lsp-java-add-import)))

(use-package robe
  :ensure t
  :hook
  (ruby-mode . robe-mode))

(use-package company
  :hook
  (scala-mode . company-mode)
  (tuareg-mode . company-mode)
  (java-mode . company-mode)
  (robe-mode . comapny-mode)
  :config
  (setq lsp-completion-provider :capf))

(use-package posframe)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package tree-sitter
  :config
  (add-hook 'cperl-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'scala-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'javascript-mode #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(defun save-and-kill-this-buffer ()
  "Save and kill buffer."
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(use-package evil
  :demand t
  :init
  (evil-mode 1)
  :config
  (evil-ex-define-cmd "wq" 'save-and-kill-this-buffer))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'cperl-mode-hook 'flycheck-mode))

;; Perl
(require 'perltidy) ; Thanks to https://github.com/zakame/perltidy.el
(require 'perl-mode)
(require 'cperl-mode)
(setq cperl-set-style "linux")
(setq cperl-highlight-variables-indiscriminately t)
(defalias 'perl-mode 'cperl-mode)
(add-hook 'before-save-hook #'(lambda ()
                                (when (or (eq major-mode 'perl-mode) (eq major-mode 'cperl-mode))
                                  (perltidy-buffer))))
(setq cperl-indent-parens-as-block t)

(eval-after-load 'cperl-mode
  '(progn
     (set-face-attribute 'cperl-array-face nil
                         :foreground  'unspecified
                         :background  'unspecified
                         :weight      'normal
                         :slant       'italic
                         )
     (set-face-attribute 'cperl-hash-face nil
                         :foreground  'unspecified
                         :background  'unspecified
                         :weight      'normal
                         :slant       'italic
                         )
     ))
(add-to-list 'auto-mode-alist '("\\.html.ep\\'" . web-mode)) ; Mojolicious templates

;; OCaml
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package dune
  :ensure t)

(use-package merlin
  :ensure t
  :hook
  ((tuareg-mode) . #'merlin-mode))

(use-package merlin-eldoc
  :ensure t
  :hook ((tuareg-mode) . merlin-eldoc-setup))

(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

(use-package utop
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(nordic-night))
 '(custom-safe-themes
   '("fa7caecc85dd0aaf60d4f74e42300a1a69f32efbad61fbd3ca26d0dcf6dfedd5" default))
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(evil-mode ivy-rich counsel dap-mode company yasnippet lsp-ui lsp-metals lsp-mode sbt-mode yaml-mode web-mode tree-sitter-langs spinner smex scala-mode s raku-mode php-mode markdown-mode magit lv json-mode ht flycheck evil dracula-theme dockerfile-mode ctrlf centaur-tabs))
 '(safe-local-variable-values
   '((eval setq flycheck-perl-include-path
           (add-to-list 'flycheck-perl-include-path
                        (concat
                         (expand-file-name
                          (locate-dominating-file default-directory ".dir-locals.el"))
                         "lib"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
