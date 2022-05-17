;;;; emacs init file

;; move Emacs generated settings to separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Don't show the splash startup screen
(setq inhibit-startup-message t)
;; other visual stuff
(blink-cursor-mode -1)
(global-hl-line-mode 1)
;; allows using the mouse in terminal
(xterm-mouse-mode 1)
;; showing line numbers
; source: https://dougie.io/emacs/indent-selection/
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Keyboard-centric user interface
(tool-bar-mode -1)
(menu-bar-mode -1)
; Don't display pop-up UI prompts
(setq use-dialog-box nil)

;; show recent files (M-x recentf-open-files)
(recentf-mode 1)
;; remember and restore last cursor location of opened files
(save-place-mode 1)

;; save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; auto update buffers when files updated on drive
(global-auto-revert-mode 1)

;;; packages

;; Define and initialize package repositories (MELPA)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use use-package to simplify loading packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; load packages

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5
	which-key-idle-secondary-delay 0.5)
  (which-key-setup-side-window-bottom))

;; Auto completion of long words
(use-package company
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 4
	company-selection-wrap-around t))
(global-company-mode)

(use-package all-the-icons
  :if (display-graphic-p))

;; Tabs
(use-package centaur-tabs
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "bar"
	  centaur-tabs-height 32
	  centaur-tabs-set-icons t
	  centaur-tabs-set-modified-marker t
	  centaur-tabs-show-navigation-buttons t
	  centaur-tabs-set-bar 'under
	  x-underline-at-descent-line t
	  centaur-tabs-modified-marker "●")
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "Calibri" 170)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  :hook
   (dashboard-mode . centaur-tabs-local-mode)
   (term-mode . centaur-tabs-local-mode)
   (calendar-mode . centaur-tabs-local-mode)
   (org-agenda-mode . centaur-tabs-local-mode)
   (helpful-mode . centaur-tabs-local-mode)
   :bind
   ("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)
   ("C-c t s" . centaur-tabs-counsel-switch-group)
   ("C-c t p" . centaur-tabs-group-by-projectile-project)
   ("C-c t g" . centaur-tabs-group-buffer-groups))

(use-package popup)

;; auto parenthesis matching
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (setq prog-mode smartparens-strict-mode))
(smartparens-global-mode t)

(use-package no-littering)

;;; setting keybinds to indent blocks of text
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

;; adding direectory with all emacs packages and the subdirectories inside it
;; requiring the packages so they are included in emacs
;; (add-to-list 'load-path "~/.emacs.d/elpa/")
;; (let ((default-directory  "~/.emacs.d/elpa"))
;;   (normal-top-level-add-subdirs-to-load-path))


;;; customized C indent formatting

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq-default c-indent-tabs-mode t     ; Pressing TAB should cause indentation
                c-indent-level 2         ; A TAB is equivilent to two spaces
                c-argdecl-indent 0       ; Do not indent argument decl's extra
                c-tab-always-indent t
                backward-delete-function nil) ; DO NOT expand tabs when deleting
  (c-add-style "my-c-style" '((c-continued-statement-offset 2))) ; If a statement continues on the next line, indent the continuation by 4
  (defun my-c-mode-hook ()
    (c-set-style "my-c-style")
    (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
    (c-set-offset 'inline-open '+)
    (c-set-offset 'block-open '+)
    (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
    (c-set-offset 'case-label '+)
    (setq c-basic-offset 2))       ; indent case labels by c-indent-level, too
  (add-hook 'c-mode-hook 'my-c-mode-hook)
  (add-hook 'c++-mode-hook 'my-c-mode-hook)

;;; customized Java indent formatting
(setq c-default-style
      '((java-mode . "ellemtel")))
(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 4)
			    tab-width 4))

 
