;;; init.el --- Rawley Fowler's Emacs Configuration
;;; Commentary:
;;; My Emacs configuration for ergonomic UNIX oriented code-editing.

(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
(setq package-enable-at-startup nil)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")
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

(set-face-attribute 'default nil :height 120)

;; Stupid bold font for no reason, BEGONE!
(mapc
   (lambda (face)
     (when (eq (face-attribute face :weight) 'bold)
       (set-face-attribute face nil :weight 'normal)))
   (face-list))

;; Don't allow C-z or C-x C-z if GUI mode
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;;
;; PACKAGES
;;

;; Package init
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Straight.el
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

(straight-use-package 'use-package) ; use-package install
(setq use-package-always-ensure t) ; always auto-install packages

;; UTILS

;; Pull correct path from dir
(use-package direnv
  :config
  (direnv-mode))

;; Evil mode
(use-package evil
  :init (evil-mode +1))

;; Project management
(use-package projectile
  :init (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Searching with ack
(use-package ack
  :config
  (global-set-key (kbd "C-c C-g") #'ack))

;; M-x crux-rename-file-and-buffer, etc.
(use-package crux
  :config
  (global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer))

;; The best linter/code checker ever made
(use-package flycheck
  :init (global-flycheck-mode))

;; Generic completion
(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

;; Extensions for ivy
(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev))

;; Git frontend
(use-package magit)

;; Upgraded searching
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Better highlighting
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

;; Debug adapter
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; 
(use-package counsel
  :after ivy
  :config (counsel-mode))

;; Positional frames
(use-package posframe)

;; Perltidy
(straight-use-package ; we have to use straight here because its not on MELPA/ELPA
 '(perltidy
   :type git
   :host github
   :repo "perl-ide/perltidy.el"
   :branch "master"))

;; MODES

;;; No config
(use-package apache-mode)
(use-package raku-mode)
(use-package caddyfile-mode)
(use-package json-mode)
(use-package dockerfile-mode)
(use-package nix-mode)
(use-package clojure-mode)
(use-package cmake-mode)
(use-package meson-mode)

;;; With config

;;;; BEGIN: web-mode

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))     ; Go, OCaml templates
  (add-to-list 'auto-mode-alist '("\\.html.ep\\'" . web-mode))  ; Mojolicious
  (add-to-list 'auto-mode-alist '("\\.ep\\'" . web-mode))       ; Mojolicious
  (add-to-list 'auto-mode-alist '("\\.tt2\\'" . web-mode))      ; Template::Toolkit
  (add-to-list 'auto-mode-alist '("\\.html.tt2\\'" . web-mode)) ; Template::Toolkit
  (add-to-list 'auto-mode-alist '("\\.tt\\'" . web-mode))       ; Template::Toolkit
  (add-to-list 'auto-mode-alist '("\\.html.tt\\'" . web-mode))  ; Template::Toolkit
  (add-to-list 'auto-mode-alist '("\\.mc\\'" . web-mode))       ; Mason
  (add-to-list 'auto-mode-alist '("\\.tx\\'" . web-mode)))      ; Text::Xslate

;;;; END: web-mode

;;;; BEGIN: cperl-mode

(setq perltidy-on-save t)
(require 'perl-mode)
(require 'cperl-mode)
(require 'projectile)
(setq cperl-set-style "linux")
(setq cperl-highlight-variables-indiscriminately t)
(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-parens-as-block t)

;; Set Perl include path
(setq flycheck-perl-include-path '())

;;; This is kinda ugly, assuming you use perlbrew its fine :)
(when (file-directory-p "~/perl5/perlbrew")
  (let ((rf-perl-version (shell-command-to-string "source ~/.bashrc && perl -e 'print($^V =~ s/v//r)' 2> /dev/null")))
    (add-to-list 'flycheck-perl-include-path (concat "~/perl5/perlbrew/perls/perl-" rf-perl-version "/lib/" rf-perl-version))))
(add-hook 'cperl-mode-hook
          (lambda ()
            (add-to-list 'flycheck-perl-include-path (concat (projectile-project-root) "lib"))))

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

;;;; END cperl-mode

(custom-set-variables)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
