;;; init.el --- Rawley Fowler's Emacs Configuration
;;; Commentary:
;;; My Emacs configuration for ergonomic UNIX oriented code-editing.
;;; Code:
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

(setq warning-minimum-level :error) ; Don't show *warnings*
(set-frame-font "UbuntuMono 14" nil t)

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

;; Need this for some reason??
(use-package flymake-jsts
  :straight '(flymake-jsts :type git :host github :repo "orzechowskid/flymake-jsts" :branch "main"))

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
(use-package cmake-mode)
(use-package meson-mode)
(use-package typescript-ts-mode)
(use-package tsx-mode
  :straight '(tsx-mode
              :type git
              :host github
              :repo "orzechowskid/tsx-mode.el"
              :branch "emacs30"))



;;; With config

;;;; BEGIN theming
(use-package all-the-icons)
(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-Iosvkem t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;;;;

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
(add-to-list 'auto-mode-alist '("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))
(setq cperl-basic-offset 2)

;;; Set Perl include path
(setq flycheck-perl-include-path '())

;;; This is kinda ugly, assuming you use perlbrew its fine :)
(when (file-directory-p "~/perl5/perlbrew")
  (let ((rf-perl-version (shell-command-to-string "source ~/.bashrc && perl -e 'print($^V =~ s/v//r)' 2> /dev/null")))
    (add-to-list 'flycheck-perl-include-path (concat "~/perl5/perlbrew/perls/perl-" rf-perl-version "/lib/" rf-perl-version))))

(add-hook 'cperl-mode-hook
          (lambda ()
            (add-to-list 'flycheck-perl-include-path (concat (projectile-project-root) "lib"))))

;;; cperl-mode does some ugly stuff
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

;;;; BEGIN corfu
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))
;;;; END corfu

;;;; BEGIN lsp
(use-package yasnippet)
(use-package which-key)
(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((cperl-mode
           tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

;;;; END lsp

(custom-set-variables)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
