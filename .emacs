;;;; emacs init file

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   ;;'("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;;; showing line numbers
;;; source: https://dougie.io/emacs/indent-selection/
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;;; setting keybinds to indent blocks of text
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

;; adding direectory with all emacs packages and the subdirectories inside it
;; requiring the packages so they are included in emacs
(add-to-list 'load-path "~/.emacs.d/elpa/")
(let ((default-directory  "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'smartparens-config)
(require 'auto-complete-config)
(require 'popup)
(require 'markdown-mode)
(require 'lsp-haskell)

(setq markdown-command "multimarkdown")

(ac-config-default)
(setq prog-mode smartparens-strict-mode)

;; turning on xterm-mouse-mode when emacs starts
;; allows using the mouse in terminal
(xterm-mouse-mode 1)

; permanently activating smartparens mode when coding in different languages
(add-hook 'java-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'c++-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'latex-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'html-mode-hook #'smartparens-mode)
(add-hook 'css-mode-hook #'smartparens-mode)

;;; customized C indent formatting

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;;; setting custom theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes '(blackboard))
 '(custom-safe-themes
   '("330503796a76a8f892677679b5a0056066146fc6799af913b012043e4b6e37c5" "9fbc61bdbab283b409204cdd10db0b943ac40c7ab1eeea72dadf0270f1e1622d" "c1b35bddcb9b59094ab444ca606e504dd6eac109856fe3c187ed203e0a804615" "4ee01899cd94c7ceeffc718865c7df852337a7f5fe42ba529dad6819c5324ecd" "2a11789555ee247b0d59f6ef71577c29805f3a464e54f94486cee62d2b6e50c4" "b7ee908b5a5b206ff359465de661796b5b6db9de801440b7759a69960298a317" "9470fd1fc0b46235669f450a7f8ea7a8b420d3f9c7906146137e753b352abf4b" "ac2d21fbbbd51256823dd924340ca25f7623f68603355bb3505218e6f46216bc" "7cc3f499b7768d1c2342fe1cb43f344a269535fdb113bc5d80d576a7eb827a12" "7889ffe28bf97778d4227cfea2c3e892d54308858afe76974341e22eba56bc5f" "af07c622d0c35189cb362bc6937e2423ae18ee3f8d881ec135ead8485639cae5" "ea70bf3020e030e2980600b19bdb5c4d035ec998a807b09a53b5f70f6bba9687" "85d34b0d251cbd8e6dc98255bd0baf9dab25a23beb4cad04e15b62a45ac5ad66" "e0becf36170a3152e16aa066c9b18681c9b292f748d5c2c4bf33b6b3560f0f8a" "54fb2d4ad496160cccfdb44ce7d5b099601528ec317a3c30f17704b493654c63" "aa23efb24d7dc5b205bd543b0ca628afd80722688f8c9d88f66c38aac64f75f4" "ac8043c08d138eb749bf04c7d8918934fe36ada621d20d421dfa5c29b28a6724" "f5836fab729668f9e0070e98e71dc7f2707124e1e058e6c1fe297afb5c2fd44a" "3dd57f590a703186e0ae52c78180975d9cec00964feb2e8acd4c92159d4cfae8" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "aad944c10b89d939059c639e6bfa0398f5db17397088fc3e5dc316c1bac57c51" "4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1" default))
 '(package-selected-packages
   '(haskell-mode lsp-ui lsp-mode flymd popup auto-complete smartparens))
 '(send-mail-function 'mailclient-send-it)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
