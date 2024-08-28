;;; init.el --- Rawley Fowler's Emacs Configuration
;;; Commentary:
;;; My Emacs configuration for ergonomic code editing in many languages.
;;; LSP, Tree-sitter, company, and Magit <3.

;;; Code:
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "ComicShannsMonoNerdFontMono-15")
  (add-to-list 'default-frame-alist '(font . "ComicShannsMonoNerdFontMono-15")))
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

; I hate that so much is bold for absolutely no reason.
(mapc
   (lambda (face)
     (when (eq (face-attribute face :weight) 'bold)
       (set-face-attribute face nil :weight 'normal)))
   (face-list))

(add-to-list 'load-path "~/.emacs.d/extras")

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'control))

(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(defun set-exec-path-from-shell-PATH ()
  "Set path to path from shell."
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(require 'package)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package autothemer :ensure t)

(straight-use-package
 '(rose-pine-emacs
   :host github
   :repo "thongpv87/rose-pine-emacs"
   :branch "master"))

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

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package nordic-night-theme
  :ensure t
  :config
  (load-theme 'nordic-night t))

(use-package apache-mode
  :ensure t)
(use-package raku-mode
  :ensure t)
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html.ep\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode)))
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
(use-package rg)

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
  (add-hook 'emacs-lisp-mode-hook #'tree-sitter-hl-mode)
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

(use-package flycheck
  :ensure t
  :config
  (add-hook 'cperl-mode-hook 'flycheck-mode))

;; Perl
(straight-use-package
 '(perltidy
   :type git
   :host github
   :repo "perl-ide/perltidy.el"
   :branch "master"))
(require 'perltidy)
(setq perltidy-on-save t)
(require 'perl-mode)
(require 'cperl-mode)
(setq cperl-set-style "linux")
(setq cperl-highlight-variables-indiscriminately t)
(defalias 'perl-mode 'cperl-mode)

;;(add-hook 'before-save-hook #'(lambda ()
;;                                (when (or (eq major-mode 'perl-mode) (eq major-mode 'cperl-mode))
;;                                  (perltidy-buffer))))
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

(setq flycheck-perl-include-path
      (add-to-list
       'flycheck-perl-include-path
       "/home/rawley/perl5/lib/perl5/"))

(use-package magit
  :ensure t)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(rose-pine-color))
 '(custom-safe-themes
   '("9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "6454421996f0508c38215a633256e36c19a28591542fb0946cfc40f1dceb89cf" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "7c7026a406042e060bce2b56c77d715c3a4e608c31579d336cb825b09e60e827" "fa7caecc85dd0aaf60d4f74e42300a1a69f32efbad61fbd3ca26d0dcf6dfedd5" default))
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(rg evil-mode ivy-rich counsel dap-mode company yasnippet lsp-ui lsp-metals lsp-mode sbt-mode yaml-mode web-mode tree-sitter-langs spinner smex scala-mode s raku-mode php-mode markdown-mode magit lv json-mode ht flycheck evil dracula-theme dockerfile-mode ctrlf centaur-tabs))
 '(safe-local-variable-values
   '((eval setq flycheck-perl-include-path
           (add-to-list 'flycheck-perl-include-path
                        (concat
                         (projectile-project-root)
                         "lib")))
     (flycheck-perl-include-path concat
                                 (projectile-project-root)
                                 "lib")
     (c-indentation-style . bsd)
     (eval setq flycheck-perl-include-path
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
