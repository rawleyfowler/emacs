;;; init.el --- Perl 5 Emacs Configuration
;;; Commentary:
;;; A Perl 5 focused Emacs configuration, for the unix genie.

;;; Code:
(server-start)

;; -- Get Font Here --
;; https://github.com/slavfox/Cozette/releases
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-16"))
(add-to-list 'default-frame-alist (list '(width . 40) '(height . 40)))

(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
(setq package-enable-at-startup nil)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-ic")
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-linum-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(recentf-mode 1)

(defun set-exec-path-from-shell ()
  "Set up Emacs' 'exec-path' and PATH environment."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell)

(add-to-list 'load-path "~/.emacs.d/extras")

;; Packages
(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defconst global/pkgs
  '(dracula-theme
    flycheck
    raku-mode
    web-mode
    json-mode
    yaml-mode
    php-mode
    dockerfile-mode
    ctrlf
    magit
    centaur-tabs
    evil
    smex
    tree-sitter
    tree-sitter-langs))
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (pkg global/pkgs)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; General Configutations
(global-flycheck-mode)
(setq flycheck-perlcritic-severity 3)

(require 'tree-sitter)
(require 'tree-sitter-langs)

(require 'ctrlf)
(ctrlf-mode +1)

(require 'ido)
(ido-mode 1)

(require 'smex)
(smex-initialize)

(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-<prior>") 'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-set-bar 'under)
(setq x-underline-at-descent-line t)
(setq centaur-tabs-enable-key-bindings t)
(centaur-tabs-change-fonts "JetBrainsMono Nerd Font" 140)

(require 'evil)
(evil-mode 1)
(defun save-and-kill-this-buffer ()
  "Save and kill buffer."
  (interactive)
  (save-buffer)(kill-current-buffer))
(evil-ex-define-cmd "wq" 'save-and-kill-this-buffer)

(defun kill-inner-word ()
  "It's ciw from Vim."
  (interactive)
  (backward-word)
  (kill-word 1))

;;; Language Specific Configurations
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
(add-hook 'cperl-mode-hook #'tree-sitter-hl-mode)
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

;; PHP
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))

;; Keys
(global-set-key (kbd "C-c C-c") #'kill-inner-word)
(global-set-key (kbd "M-x") #'smex)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(delete-selection-mode nil)
 '(package-selected-packages '(raku-mode docker-compose-mode flycheck dracula-theme))
 '(safe-local-variable-values
   '((eval let
           ((project-dir
             (expand-file-name
              (locate-dominating-file default-directory ".dir-locals.el"))))
           (setq-local flycheck-perl-include-path
                       (list
                        (concat project-dir "lib")
                        (concat project-dir "local/lib/perl5/")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here.
