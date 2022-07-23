;; setting encoding system
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)              
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; move Emacs generated settings to separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; font size/style

(setq fixed-pitch-font "DejaVu Sans Mono"
      variable-pitch-font "Cambria")

(set-face-attribute 'default nil
                    :font fixed-pitch-font
                    :height 130)
;; set fixed pitch face font
(set-face-attribute 'fixed-pitch nil
                    :font fixed-pitch-font
                    :height 130)
;; set the variable pitch face font
(set-face-attribute 'variable-pitch nil
                    :font variable-pitch-font
                    :height 130
                    :weight 'regular)

;; showing line numbers
(column-number-mode)
(global-display-line-numbers-mode)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; allows using the mouse in terminal
(xterm-mouse-mode 1)

;; Keyboard-centric user interface
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
; Don't display pop-up UI prompts
(setq use-dialog-box nil)

;; Don't show the splash startup screen
(setq inhibit-startup-message t)

(blink-cursor-mode -1)
(global-hl-line-mode 1)

;; auto update buffers when files updated on drive
(global-auto-revert-mode 1)

;; show recent files (M-x recentf-open-files)
(recentf-mode 1)

;; remember and restore last cursor location of opened files
(save-place-mode 1)

;; save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;;; packages

;; Define and initialize package repositories (MELPA/ELPA)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; use use-package to simplify loading packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.5)
  (which-key-setup-side-window-bottom))

(use-package no-littering)

;; better help pages
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
)

(use-package ivy
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; give description of commands in counsel-M-x
(use-package ivy-rich
  :after ivy counsel
  :init
  (ivy-rich-mode 1))

(use-package popup)

;; Auto completion of words
(use-package company
  :after popup
  :config
  (setq company-idle-delay 0
        ;; minimum word length before it gives suggestions
        company-minimum-prefix-length 4
        company-selection-wrap-around t))
(global-company-mode)

;; auto parenthesis matching
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (setq prog-mode smartparens-strict-mode))
(smartparens-global-mode t)

(use-package all-the-icons
  :if (display-graphic-p))

;; Tabs
(use-package centaur-tabs
  :after evil
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
  (centaur-tabs-change-fonts variable-pitch-font 150)
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
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-material t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; use emacs keybinding when editing files (insert mode)
(setq evil-disable-insert-state-bindings t)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  ;; show diff in same window
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; make magit prompt for ssh password
(use-package ssh-agency)
(setenv "SSH_ASKPASS" "git-gui--askpass")

(defun ska/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun ska/org-mode-setup ()
  "Set up org mode"
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  ;; scale font size of headers
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
                        :font variable-pitch-font
                        :weight 'regular
                        :height (cdr face))))

(defun ska/org-mode-toggle-hide-emphasis-markers ()
  "Toggle org mode emphasis markers on and off"
  (interactive)
  (progn (if (null org-hide-emphasis-markers)
             (setq org-hide-emphasis-markers t)
           (setq org-hide-emphasis-markers nil))
         (org-mode)))

(use-package org
  :hook (org-mode . ska/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (ska/org-font-setup)
  :bind (
         ;; toggle org mode emphasis markers on and off
         ("C-c s e" . ska/org-mode-toggle-hide-emphasis-markers)))

(setq org-directory (concat (getenv "HOME") "/OneDrive - University of Illinois - Urbana/OrgRoamNotes"))

(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

;; make headings in orgmode look nicer
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun ska/org-mode-visual-fill ()
  "Pads both sides of text buffer (looks more like a word processor)"
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; add padding to sides of buffer
(use-package visual-fill-column
  :after org
  :hook (org-mode . ska/org-mode-visual-fill))

(use-package htmlize)

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; allows creating new node on page without opening it (stay on same file after inserting link to new file)
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(use-package org-roam
  :after org
  :ensure t
  :init
  ;; suppress v2 upgrade warning
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename org-directory))
  (org-roam-completion-everywhere t)
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (capitalize
         (file-name-nondirectory
          (directory-file-name
           (file-name-directory
            (file-relative-name (org-roam-node-file node) org-roam-directory)))))
      (error "")))

  (setq org-roam-node-display-template (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; creating capture templates for org-roam notes
  ;; source: https://jethrokuan.github.io/org-roam-guide/
  (setq org-roam-capture-templates
        '(("i" "Idea" plain "%?"
           :if-new (file+head "idea/$%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+filetags: :idea:\n")
           :immediate-finish t
           :unarrowed t)
          ("r" "Reference Material")
          ("rr" "Paper / Website" plain "%?"
           :if-new (file+head "reference/paper/$%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+filetags: :reference:\n")
           :immediate-finish t
           :unarrowed t)
          ("rc" "Course Material")
          ("rcn" "Course Notes (lecture, textbook, etc.)" plain "%?"
           :if-new (file+head "reference/course/notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+filetags: :reference:\n")
           :immediate-finish t
           :unarrowed t)
          ("rci" "Course Index" plain "%?"
             :if-new (file+head "reference/course/index/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+filetags: :reference:\n")
             :immediate-finish t
             :unarrowed t)

          ("a" "Article" plain "%?"
           :if-new (file+head "articles/$%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+filetags: :article:\n")
           :immediate-finish t
           :unarrowed t)))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         (:map org-mode-map
               (("C-M-i" . completion-at-point)
                ("C-c n l" . org-roam-buffler-toggle)
                ("C-c n t" . org-roam-tag-add)))))

(use-package deft
  :after org
  :config
  (setq deft-directory org-directory
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title nil)
  :bind
  ("C-c n d" . deft))

;; dependencies for org-roam-ui
(use-package websocket)
(use-package simple-httpd)


(use-package org-roam-ui
  :after websocket simple-httpd org-roam
  :bind (
         ("C-c n u i" . org-roam-ui-open)))

(setq bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                    ("file" "Link to document file." ":"))
      bibtex-align-at-equal-sign t)

(setq bib-files-directory (directory-files org-directory t "^[A-Z|a-z].+.bib")
      pdf-files-directory (concat (getenv "HOME") "/OneDrive - Univesity of Illinois - Urbana/Papers"))

(use-package ivy-bibtex
  :after ivy
  :config
  (setq bibtex-completion-bibliography bib-files-directory
        bibtex-completion-library-path pdf-files-directory
        bibtex-completion-pdf-field "File"
        bibtex-completion-notes-path org-directory
        bibtex-completion-additional-search-fields '(keywords))
  :bind
  (("C-c b" . ivy-bibtex)))

(use-package citar
  :after org ;; depends on org-directory
  :bind (("C-c n b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography (concat (file-truename org-directory) "/biblio.bib")))

(defun ska/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

;; eshell comes with Emacs. This allows us to configure it
(use-package eshell
  :hook (eshell-first-time-mode . ska/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  (eshell-git-prompt-use-theme 'robbyrussell))

(use-package eshell-git-prompt
  :after eshell)

;; automatically tangle emacs config org file when saving
(defun ska/org-babel-tangle-config ()
  "Tangle code in org file when the file is saved if the file is the Emacs config file"
  (when (string-equal (buffer-file-name)
                      (file-truename "~/.emacs.d/init.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (
                          lambda () (add-hook 'after-save-hook #'ska/org-babel-tangle-config)))

;; source: https://jethrokuan.github.io/org-roam-guide/
(defun ska/org-roam-node-from-cite (keys-entries)
  "Create an org roam node from the Citar bibliography."
  (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
  (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                              "${author editor} :: ${title}")))
    (org-roam-capture- :templates
                       '(("rr" "reference" plain "%?" :if-new
                          (file+head "reference/paper/${citekey}.org"
                                     ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n#+author: Shayan Azmoodeh\n#+filetags: :reference:\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))

(defun ska/delete-file-and-buffer ()
  "Deletes file open in current buffer and kills window
if the current buffer contains a file"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename ; if curr buff contains a file
        (progn (if (vc-backend filename) ; if version control contains the file
                   (vc-delete filename)
                 (delete-file filename))
               (message "Deleted file %s" filename)
               (kill-buffer))
      (message "Current buffer does not contain a file"))))

;;; customized C indent formatting

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
