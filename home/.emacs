;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load External Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/")

(load "package-settings") ; el-get settings
(load "font-settings")	  ; default fonts and font cycle script
(load "latex-settings")	  ; add hooks for latex

(require 'multiple-cursors)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode 1)			; show the menu...
(tool-bar-mode -1)			; ... but not the the toolbar
(ruler-mode -1)				; kein Lineal
(tabbar-mode t)				; Tabbar mode
(iswitchb-mode t)			; Show auto completin in buffer menu..
(icomplete-mode t)			; and other minibuffer menus
(setq icomplete-prospects-height 1)	; ...only one line
;(partial-completion-mode t)		; show partial results at tab-completion

(scroll-bar-mode t)			; show a scrollbar...
(set-scroll-bar-mode 'right)		; ... on the right
(setq scroll-margin 1			; do smooth scrolling, ...
      scroll-conservatively 100000	; ... the defaults ...
      scroll-up-aggressively 0.01	; ... are very ...
      scroll-down-aggressively 0.01)	; ... annoying

(when (fboundp 'set-fringe-mode)	; emacs22+ 
  (set-fringe-mode 2))			; space left of col1 in pixels

(transient-mark-mode t)			; make the current 'selection' visible
(delete-selection-mode -1)		; do not delete the selection with a keypress
(setq x-select-enable-clipboard t	; copy-paste should work ...
      interprogram-paste-function	; ...with...
      'x-cut-buffer-or-selection-value)	; ...other X clients


					; Work with visible lines not with (wrapped,) logical lines 
(visual-line-mode 0)    		; 1 for on, 0 for off.

(setq fill-column 80)			; Break lines at colum x

(set-face-attribute 'default nil :height 120) ; set default font size

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(windmove-default-keybindings)				; switch windows using <s-{arrow keys}>
(global-set-key (kbd "s-o") 'other-window)              ; quick window change (s=WinKey)
(global-set-key (kbd "C-x $") 'ispell-buffer)		; spell check
(global-set-key (kbd "C-x !") 'eshell)	         	; eshell
(global-set-key (kbd "C-x a") 'align-regexp)       	; align
(global-set-key (kbd "C-x c") 'calendar)		; show calendar
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)	; warp long lines
(global-set-key (kbd "C-\\") 'dabbrev-expand)		; auto complete? (=M-/)

;; function keys
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<f9>") 'cycle-font)	        ; cf. emacs.d/font-settings.el
(global-set-key (kbd "M-<f5>") 'revert-buffer)          ; revert buffer from file
(global-set-key (kbd "M-<f11>") 'menu-bar-mode)		; toggle menubar
(global-set-key (kbd "M-<f12>") 'tabbar-mode)		; toggle tabbar

;; Multiple cursurs default key bindings
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Tabbar mode navigation
(global-set-key (kbd "M-<left>") 'tabbar-backward-tab)
(global-set-key (kbd "M-<right>") 'tabbar-forward-tab)

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

(global-unset-key (kbd "C-z"))				; disable suspend

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n promting

(defalias 'indent-regexp 'align-regexp)
(defalias 'ar 'align-regexp)

(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'sc 'ispell-buffer)

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ee 'eval-expression)
(defalias 'elm 'emacs-lisp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'calendar-today-visible-hook 'calendar-mark-today) ; Mark today in calendar mode
(add-hook 'diary-display-hook 'diary-fancy-display) ;; include org agenda in diary
(setq auto-mode-alist (cons '("\\.htmpl$" . html-mode) auto-mode-alist)) ;; Autoload html for htmlp files (MI webdir)

;; Natural keybindings for iswichtb
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; Omit hidden files in dired mode
(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq bol "." (not (any "."))) ;; dot-files
              (seq "~" eol)                 ;; backup-files
              (seq bol "CVS" eol)           ;; CVS dirs
              )))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))



(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

;; Encrytion settings
(require 'epa-file)
(epa-file-enable) ;; auto encrypt gpg files

;; JEDI PYTHON EDITOR
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

(add-hook 'python-mode-hook 'jedi:ac-setup)

;; Markdown/R-files R.md
(defun markdown-r-keys ()
  "my keys for `Markdown/R mode'."
  (interactive)
  (local-set-key (kbd "C-c C-e") 'ess-eval-paragraph)
  )
(add-hook 'markdown-mode-hook 'markdown-r-keys)

;; ORG MODE
(add-to-list 'load-path "~/git/org-reveal")
(add-to-list 'load-path "~/git/org-mode/lisp")
(add-to-list 'load-path "~/git/org-mode/contrib/lisp" t)
(require 'ox-beamer)
(require 'ox-md)
(require 'ox-deck)
(require 'ox-reveal)
(require 'ox-taskjuggler)


;; DGDB MODE
(setq gdb-show-main 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (wombat)))
 '(inhibit-startup-screen t)
 '(lua-indent-level 2)
 '(markdown-css-path "http://kevinburke.bitbucket.org/markdowncss/markdown.css")
 '(markdown-xhtml-header-content "<style> p { text-align:
 justify; } </style>")
 '(show-trailing-whitespace t)
 '(todotxt-file "/home/hartmann/Dropbox/todo/todo.txt" nil (todotxt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face))))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil))))
