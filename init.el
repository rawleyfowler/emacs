;;; init.el --- Rawley Fowler's Emacs Configuration
;;; Commentary:
;;; My Emacs configuration for ergonomic code editing in many languages.
;;; LSP, Tree-sitter, company, and Magit <3.

;;; Code:
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Hurmit Nerd Font Mono-15")
  (add-to-list 'default-frame-alist '(font . "Hurmit Nerd Font Mono-15")))
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
(electric-pair-mode +1)
(recentf-mode 1)
(global-display-line-numbers-mode 1)
(add-to-list 'image-types 'svg)
(setq require-final-newline t)

(add-to-list 'load-path "~/.emacs.d/extras")

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'control))

(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(require 'package)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-hook 'c++-mode-hook (lambda () (c-set-offset 'innamespace [0])))

(require 'use-package)

(use-package ack
  :ensure t
  :config
  (global-set-key (kbd "C-c C-g") #'ack))

(use-package crux
  :ensure t
  :config
  (global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map
    (kbd "C-c p")
    'projectile-command-map))

(use-package treemacs
  :ensure t)
(use-package treemacs-evil
  :after treemacs
  :ensure t)
(use-package treemacs-projectile
  :after treemacs
  :ensure t)

(use-package nordic-night-theme
  :ensure t
  :config
  (load-theme 'nordic-night t))

(use-package apache-mode
  :ensure t)
(use-package raku-mode
  :ensure t)
(use-package web-mode
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(use-package nix-mode
  :ensure t)

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

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package go-mode
  :ensure t
  :hook
  (before-save . gofmt-before-save)
  :config
  (setq tab-width 4))

(use-package lsp-mode
  :hook
  (go-mode . lsp)
  (scala-mode . lsp)
  (java-mode . lsp)
  (ruby-mode . lsp)
  (tuareg-mode . lsp)
  (clojure-mode . lsp)
  (clojurec-mode . lsp)
  (clojurescript-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :init
  (setq lsp-completion-provider 'company-mode)
  (setq lsp-completion-enable t)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-signature-render-document nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-treemacs
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-minimum-prefix-length 1)
  :after lsp-mode
  :ensure t)

(defun start-ccls ()
  "Start ccls function for hooks."
  (require 'ccls)
  (lsp-deferred))

(use-package ccls
  :hook
  ((c-mode c++-mode objc-mode cuda-mode) . #'start-ccls))

(use-package cmake-mode
  :ensure t)
(use-package meson-mode
  :ensure t)

(use-package lsp-metals)
(use-package lsp-ui
  :init
  (setq lsp-ui-sideline-show-code-actions nil))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode +1))

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
  (robe-mode . company-mode))

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
  (add-hook 'javascript-mode #'tree-sitter-hl-mode)
  (add-hook 'ruby-mode #'tree-sitter-hl-mode)
  (add-hook 'perl-mode #'tree-sitter-hl-mode))

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
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode)) ; Go templates

;; OCaml
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package dune
  :ensure t)

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

(use-package utop
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(use-package magit
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(nordic-night))
 '(custom-safe-themes
   '("7c7026a406042e060bce2b56c77d715c3a4e608c31579d336cb825b09e60e827" "fa7caecc85dd0aaf60d4f74e42300a1a69f32efbad61fbd3ca26d0dcf6dfedd5" default))
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
