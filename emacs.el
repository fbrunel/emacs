;;;; .emacs

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(add-log-mailing-address nil)
 '(browse-url-browser-function (quote browse-url-galeon))
 '(browse-url-galeon-new-window-is-tab t)
 '(browse-url-new-window-flag t)
 '(canlock-password "7cd01b64aef323dbbda6f2378bc925862bf5946a")
 '(column-number-mode t)
 '(compilation-window-height 10)
 '(current-language-environment "Latin-1")
 '(cursor-in-non-selected-windows nil)
 '(default-frame-alist (quote ((foreground-color . "cornsilk") (background-color . "#21515c") (cursor-color . "white") (vertical-scroll-bars . right) (tool-bar-lines . 0) (menu-bar-lines . 1))))
 '(delete-selection-mode t nil (delsel))
 '(diary-file "~/.emacs.d/diary")
 '(dired-listing-switches "-l")
 '(erc-nick "Bloody")
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
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(slime-conservative-indentation nil)
 '(slime-startup-animation nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines t)
 '(user-full-name "Frederic Brunel")
 '(user-mail-address "fbrunel@gmail.com"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#21515c" :foreground "cornsilk" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "andale mono"))))
 '(cursor ((t (:background "white"))) t)
 '(font-lock-builtin-face ((t (:foreground "OliveDrab1"))))
 '(font-lock-comment-face ((t (:foreground "RosyBrown1" :background "#005f68"))))
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

;;; Load paths
(require 'cl)

(dolist (file (directory-files (expand-file-name "~/Library/Emacs/") t "[a-z]"))
  (pushnew file load-path))
(pushnew "/usr/local/bin" exec-path)

;;; Modes

(require 'psvn)

;; CEDET
;(require 'cedet)
;(setq semanticdb-default-save-directory "/Users/fbrunel/.emacs.d/semantic/")
;(semantic-load-enable-code-helpers)

;; Rails

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(setq auto-mode-alist  (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rhtml$" . html-mode) auto-mode-alist))
(modify-coding-system-alist 'file "\\.rb$" 'utf-8)
(modify-coding-system-alist 'file "\\.rhtml$" 'utf-8)

;(require 'rails)

;; JDE
;(require 'jde)
;(add-hook 'jde-mode-hook 'turn-on-auto-fill)

;; ibuffer
(require 'ibuffer)
(global-set-key [(control x) (control b)] 'ibuffer)

;; Bookmark
;; (require 'bm)
;; (global-set-key [C-f2] 'bm-toggle-bookmark)
;; (global-set-key [f2] 'bm-goto-bookmark)
;; (global-set-key [S-f2] 'bm-goto-bookmark-previous)

;; Lisp
;; (require 'slime)
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; (setq lisp-indent-function 'common-lisp-indent-function)

;; Text mode
;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; CSS mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)

;;; Globals keybindings
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

;;;; .emacs ends here
