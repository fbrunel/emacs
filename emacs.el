;;;; .emacs

;;; Paths

(setq *search-dir* (expand-file-name "~/Library/Emacs/vendor"))
(setq *dotfiles-dir* (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path *search-dir*)

;;; Default librairies

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;;; Modes

(require 'magit)

;; Java

;(require 'cedet)
;(setq semanticdb-default-save-directory (concat *dotfiles-dir* "semantic/"))
;(semantic-load-enable-code-helpers)
;(require 'jde)
;(add-hook 'jde-mode-hook 'turn-on-auto-fill)

;; Lisp
;; (require 'slime)
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; (setq lisp-indent-function 'common-lisp-indent-function)

;; Autoloads

(autoload 'ruby-mode "ruby-mode")
(autoload 'css-mode "css-mode")

;; Associate modes with file extensions

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;;; Keybindings

(global-set-key [dead-circumflex] 'compose-circumflex-map)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [C-kp-home] 'beginning-of-buffer)
(global-set-key [C-kp-end] 'end-of-buffer)
(global-set-key [C-return] 'kill-this-buffer)
(global-set-key [C-tab] 'other-window)
(global-set-key [C-backspace] 'comment-region)
(global-set-key [f5] 'jde-ant-build)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [f10] 'jde-javadoc-checkdoc-at-line)
(global-set-key [f11] 'grep-find)
(global-set-key [C-f12] 'cvs-update)
(global-set-key [f12] 'add-change-log-entry)
(global-set-key [C-kp-add] 'enlarge-window)
(global-set-key [C-kp-subtract] 'shrink-window)
(global-set-key [kp-delete] 'delete-char)

(define-key global-map [(delete)] "\C-d")
(define-key global-map "\M-g" 'goto-line)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x g") 'magit-status)

;;; Customization

(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(add-log-mailing-address nil)
 '(auto-compression-mode t)
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(compilation-window-height 10)
 '(current-language-environment "Latin-1")
 '(cursor-in-non-selected-windows nil)
 '(cursor-type (quote (bar . 1)) t)
 '(delete-selection-mode t nil (delsel))
 '(dired-listing-switches "-l")
 '(erc-nick "fredb")
 '(erc-server "ircnet.grolier.net")
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-group-mode-hook (quote (gnus-topic-mode)))
 '(gnus-home-directory "~/.emacs.d/")
 '(gnus-select-method (quote (nntp "news.free.fr")))
 '(gnus-summary-line-format "%U%R%z%I%(%[%4t: %-23,23f%]%) %s
")
 '(gnus-thread-hide-subtree t)
 '(gnus-treat-body-boundary (quote head))
 '(ibuffer-formats (quote ((mark modified read-only " " (name 32 32) " " (size 6 -1 :right) " " (mode 16 16 :right) " " filename) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-show-empty-filter-groups nil)
 '(icomplete-mode t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell" t)
 '(jde-ant-enable-find t)
 '(jde-ant-read-target t)
 '(jde-enable-abbrev-mode t)
 '(jde-gen-cflow-enable nil)
 '(jde-javadoc-author-tag-template "\"* @author \" user-full-name")
 '(jde-run-mode-hook nil)
 '(mail-default-directory "~/.emacs.d/Mail/")
 '(mail-source-directory "~/.emacs.d/Mail/")
 '(mail-user-agent (quote message-user-agent))
 '(read-mail-command (quote gnus))
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(slime-conservative-indentation nil)
 '(slime-startup-animation nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines t)
 '(user-full-name "Frederic Brunel")
 '(user-mail-address "fbrunel@gmail.com")
 '(visible-bell t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#211e1e" :foreground "cornsilk" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Inconsolata"))))
 '(cursor ((t (:background "white"))))
 '(font-lock-builtin-face ((t (:foreground "OliveDrab1"))))
 '(font-lock-comment-face ((t (:foreground "#555555" :background "#211e1e"))))
 '(font-lock-constant-face ((t (:foreground "Violet"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "CornFlowerBlue"))))
 '(font-lock-keyword-face ((t (:foreground "LightSteelBlue1"))))
 '(font-lock-string-face ((t (:foreground "Salmon"))))
 '(font-lock-type-face ((t (:foreground "Gold1"))))
 '(font-lock-variable-name-face ((t (:foreground "Aquamarine"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "Pink"))))
 '(highlight ((t (:background "slate blue"))))
 '(ido-first-match ((t (:foreground "lightsteelblue1" :weight bold))))
 '(ido-first-match-face ((t (:foreground "lightsteelblue1" :weight bold))))
 '(ido-only-match ((((class color)) (:foreground "RosyBrown1"))))
 '(ido-only-match-face ((((class color)) (:foreground "RosyBrown1"))))
 '(ido-subdir ((((class color)) (:foreground "skyblue" :weight bold))))
 '(ido-subdir-face ((((class color)) (:foreground "skyblue" :weight bold))))
 '(info-menu-5 ((t (:bold t :underline t :foreground "yellow"))))
 '(info-node ((t (:bold t :italic t :underline t :foreground "cyan"))))
 '(info-xref ((t (:bold t :italic t :underline t :foreground "cornflowerblue"))))
 '(jde-java-font-lock-modifier-face ((((class color) (background dark)) (:foreground "OliveDrab1"))))
 '(message-cited-text-face ((((class color) (background dark)) (:foreground "gray"))))
 '(message-header-cc-face ((t (:foreground "LightBlue3" :weight bold))))
 '(message-header-name-face ((((class color) (background dark)) (:foreground "LightGreen"))))
 '(message-header-other-face ((((class color) (background dark)) (:foreground "green3"))))
 '(message-header-subject-face ((((class color) (background dark)) (:foreground "LightBlue"))))
 '(message-header-to-face ((t (:foreground "LightBlue" :weight bold))))
 '(message-separator-face ((((class color) (background dark)) (:foreground "cyan3"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "LightSteelBlue" :foreground "black"))))
 '(paren-face-match-light ((((class color)) nil)))
 '(region ((t (:background "Steel Blue"))))
 '(show-paren-match ((((class color)) (:background "#005fbb"))))
 '(slime-repl-inputed-output-face ((((class color) (background dark)) (:foreground "darkgray"))))
 '(speedbar-highlight-face ((((class color) (background dark)) (:background "sea green")))))

;;;; .emacs ends here