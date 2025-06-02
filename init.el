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

(setq mouse-wheel-scroll-amount '(0.01))
(setq mouse-wheel-progressive-speed nil)

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
;; Emacs 29 includes use-package built-in so I'm pretty sure the lines below are legacy
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package gnu-elpa-keyring-update)

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
         ("C-c a s" . swiper-all)
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

(use-package smooth-scrolling
  :init (smooth-scrolling-mode 1))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package origami
  :ensure t
  :config
  (global-origami-mode))

(use-package treemacs
  :ensure t
  :init
  (global-unset-key (kbd "C-x t t")) ; Unbind tab-bar key
  :bind (("C-x t t" . treemacs)
         ("M-0" . treemacs-select-window))
  :config
  ;; so the treemacs frame isn't part of the frame switching cycle
  (setq treemacs-is-never-other-window t))

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
  (setq visual-fill-column-width 200
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
  (org-roam-database-connector 'sqlite-builtin) ; new in Emacs 29, uses built-in sql support
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
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+date: %u\n#+lastmod: \n#+filetags: :idea:\n")
           :immediate-finish t
           :unarrowed t)
          ("r" "Reference Material")
          ("rr" "Paper / Website" plain "%?"
           :if-new (file+head "reference/paper/$%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+date: %u\n#+lastmod: \n#+filetags: :reference:\n")
           :immediate-finish t
           :unarrowed t)
          ("ri" "Index" plain "%?"
           :if-new (file+head "reference/index/$%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+date: %u\n#+lastmod: \n#+filetags: :reference:\n")
           :immediate-finish t
           :unarrowed t)
          ("rc" "Course Material")
          ("rcn" "Course Notes (lecture, textbook, etc.)" plain "%?"
           :if-new (file+head "reference/course/notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+date: %u\n#+lastmod: \n#+filetags: :reference:\n")
           :immediate-finish t
           :unarrowed t)
          ("rci" "Course Index" plain "%?"
           :if-new (file+head "reference/course/index/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+date: %u\n#+lastmod: \n#+filetags: :reference:\n")
           :immediate-finish t
           :unarrowed t)

          ("a" "Article" plain "%?"
           :if-new (file+head "articles/$%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+author: Shayan Azmoodeh\n#+date: %u\n#+lastmod: \n#+filetags: :article:\n")
           :immediate-finish t
           :unarrowed t))
        time-stamp-active t
        time-stamp-start "#\\+lastmod: [ \t]*"
        time-stamp-end "$")
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         (:map org-mode-map
               (("C-M-i" . completion-at-point)
                ("C-c n l" . org-roam-buffer-toggle)
                ("C-c n t" . org-roam-tag-add)))))

(add-hook 'before-save-hook 'time-stamp nil)

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
      pdf-files-directory (concat (getenv "HOME") "/Zotero/storage"))

(use-package ivy-bibtex
  :after ivy
  :config
  (setq bibtex-completion-bibliography bib-files-directory
        bibtex-completion-library-path pdf-files-directory
        bibtex-completion-pdf-field "File"
        bibtex-completion-notes-path org-directory
        bibtex-completion-additional-search-fields '(keywords))
  :bind
  (("C-c B" . ivy-bibtex)))

(use-package org-ref
    :config
    (require 'org-ref-ivy)
    (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
          org-ref-insert-cite-function 'org-ref-cite-insert-ivy
          org-ref-insert-label-function 'org-ref-insert-label-link
          org-ref-insert-ref-function 'org-ref-insert-ref-link
          org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    :bind (:map org-mode-map
                ("C-c ]" . org-ref-insert-link)
                ("s-[" . org-ref-insert-link-hydra/body)))

(use-package org-roam-bibtex
  :after (org-roam ivy-bibtex)
  :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
  :config
  (require 'org-ref))

(org-roam-bibtex-mode)

(use-package citar
  :after org ;; depends on org-directory
  :config
  (setq citar-notes-paths
          (list (concat (file-truename org-directory) "/reference/paper")))
  :custom
  (org-cite-global-bibliography (directory-files org-directory t "^[A-Z|a-z].+.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-org-roam-mode 1) ; enable mode on startup
  :bind
  (("C-c b c" . #'citar-create-note)
   ("C-c b o" . #'citar-open-notes)
  (:map org-mode-map
        :package org ("C-c b i" . #'org-cite-insert))))

(use-package citar-org-roam
  :after (org-roam org-roam-bibtex citar)
  :config
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam Notes"
                           :category 'org-roam-node
                           :items #'citar-org-roam--get-candidates
                           :hasitems #'citar-org-roam-has-notes
                           :open #'citar-org-roam-open-note
                           :create #'orb-citar-edit-note
                           :annotate #'citar-org-roam--annotate))
  (setq citar-org-roam-subdir "reference/paper")
  :custom
  (setq citar-notes-source 'orb-citar-source))

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

(use-package
  exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package eshell-git-prompt
  :after eshell)

(use-package python-black
  :demand t
  :after python
  :hook
  (python-mode . python-black-on-save-mode-enable-dwim))

(use-package conda
  :ensure t
  :custom
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode f))

;; also change the default python command (pythonic is used under the hood by conda)
(setq pythonic-interpreter "python")

(use-package anaconda-mode
  :ensure t
  :after conda
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package pyimpsort
  :config (eval-after-load 'python
            '(define-key python-mode-map "\C-c\C-u" #'pyimpsort-buffer)))

(use-package auctex
  :ensure t
  :defer t
  :custom (setq TeX-parse-self t
                TeX-auto-save t))

(use-package yaml-mode
  :hook yaml-mode . (lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(use-package dockerfile-mode)

(use-package 2048-game)

(use-package sudoku)

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
  (interactive (list (citar-select-refs :multiple nil)))
  (let ((title "${author} :: ${title}"))
    (org-roam-capture- :templates
                       '(("rr" "reference" plain "%?" :if-new
                          (file+head "reference/paper/${citekey}.org"
                                     ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n#+author: Shayan Azmoodeh\n#+date: %u\n#+lastmod: \n#+filetags: :reference:\n")
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
                   (vc-delete-file filename)
                 (delete-file filename))
               (message "Deleted file %s" filename)
               (kill-buffer))
      (message "Current buffer does not contain a file"))))

(defun ska/open-init-file ()
    "Opens the Emasc init.org file (note this is the *org* file, not the raw el file)."
  (interactive)
  (find-file
   (concat (file-name-sans-extension user-init-file) ".org")))

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

(setq python-shell-interpreter "python")

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)
            (python-indent-guess-indent-offset)))
