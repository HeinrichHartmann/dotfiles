;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode 1)			; show the menu...
(tool-bar-mode -1)			; ... but not the the toolbar
(ruler-mode -1)				; kein Lineal
;;(tabbar-mode t)				; Tabbar mode
(iswitchb-mode t)			; Show auto completin in buffer menu..
(icomplete-mode t)			; and other minibuffer menus
(setq icomplete-prospects-height 1)	; ...only one line
;(partial-completion-mode t)		; show partial results at tab-completion

(set-face-attribute 'default nil :height 120)

(scroll-bar-mode t)			; show a scrollbar...
(set-scroll-bar-mode 'right)		; ... on the right
(setq scroll-margin 1			; do smooth scrolling, ...
      scroll-conservatively 100000	; ... the defaults ...
      scroll-up-aggressively 0.01	; ... are very ...
      scroll-down-aggressively 0.01)	; ... annoying

(when (fboundp 'set-fringe-mode)	; emacs22+ 
  (set-fringe-mode 2))			; space left of col1 in pixels
					; Work with visible lines not with (wrapped,) logical lines 
(visual-line-mode 0)    		; 1 for on, 0 for off.

(setq fill-column 80)			; Break lines at colum x

(transient-mark-mode t)			; make the current 'selection' visible
(delete-selection-mode -1)		; do not delete the selection with a keypress
(setq x-select-enable-clipboard t	; copy-paste should work ...
      interprogram-paste-function	; ...with...
      'x-cut-buffer-or-selection-value)	; ...other X clients

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "s-o") 'other-window)              ; quick window change (s=WinKey)
(global-set-key (kbd "C-x $") 'ispell-buffer)		; spell check
(global-set-key (kbd "C-x !") 'eshell)	         	; eshell
(global-set-key (kbd "C-x C-a") 'align-regexp)		; align regexp : indent
(global-set-key (kbd "C-x a") 'align)       		; align 
;; (global-unset-key "\C-z")				; disable suspend
(global-set-key (kbd "C-x c") 'calendar)		; show calendar
(global-set-key (kbd "M-<f11>") 'menu-bar-mode)		; show menubar
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)	; warp long lines
;(global-unset-key "\C-\\")                             ; disable input method switch
(global-set-key (kbd "C-\\") 'dabbrev-expand)		; auto complete? (=M-/)
(global-set-key (kbd "M-<f5>") 'revert-buffer)          ; quick window change (s=WinKey)

(windmove-default-keybindings)				; switch windows using <s-{arrow keys}>

(global-set-key [f11] 'toggle-fullscreen)
(global-set-key [f9] 'cycle-font)	; cycle fonts with F9

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load External Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/")

;;(load "font-settings")	; default fonts and font cycle script
;;(load "latex-settings")	; add hooks for latex


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
 justify; } </style>"))
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
