;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load External Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "helpers")          ; convenicence functions
(load "package-settings") ; el-get settings
(load "tmux")
(load "font-settings")	  ; default fonts and font cycle script
(load "latex-settings")	  ; add hooks for latex

;; install default set of packages
(ensure-package-installed
 'magit
 'jedi
 'markdown-mode
 'multiple-cursors
 'el-get
 'visual-regexp-steroids
)

(package-initialize)

(require 'multiple-cursors)
(require 'visual-regexp-steroids)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default major-mode 'text-mode) ; edit files in text mode per default

(menu-bar-mode -1)			; show the menu...
(tool-bar-mode -1)			; ... but not the the toolbar
(ruler-mode -1)				; kein Lineal
(tabbar-mode -1)				; Tabbar mode
(icomplete-mode t)			; Show auto completin in minibuffer menus
(setq icomplete-prospects-height 1)	; ...only one line

(scroll-bar-mode -1)			; show no scrollbar...
;(set-scroll-bar-mode 'right)		; ... on the right
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
(visual-line-mode 1)    		; 1 for on, 0 for off.

(setq fill-column 80)			; Break lines at colum x

(set-face-attribute 'default nil :height 120) ; set default font size
(setq-default show-trailing-whitespace 1) ; mark whitespace at the end of the line

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(savehist-mode 1) ; keep M-x History

(setq set-mark-command-repeat-pop 1) ; cycle through marks with a single C-SPC, after the first C-u C-SPC was used.


;; show fill directoy in frame title

(setq frame-title-format
      (list (format "%s %%S: %%j" (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


;; AutoSave files in /tmp/
;; http://emacswiki.org/emacs/AutoSave
;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(windmove-default-keybindings)				; switch windows using <s-{arrow keys}>

(global-set-key (kbd "C-x $") 'ispell-buffer)		; spell check
(global-set-key (kbd "C-x !") 'hell)	         	; eshell
(global-set-key (kbd "C-x a") 'align-regexp)       	; align
(global-set-key (kbd "C-x c") 'calendar)		; show calendar
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)	; warp long lines
(global-set-key (kbd "C-\\") 'dabbrev-expand)		; auto complete? (=M-/)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; don't use arrow keys for cursor movement
(global-set-key (kbd "<left>") 'shrink-window-horizontally)
(global-set-key (kbd "<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<down>") 'shrink-window)
(global-set-key (kbd "<up>") 'enlarge-window)


;;
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)


;; function keys

(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<f9>") 'cycle-font)	        ; cf. emacs.d/font-settings.el
(global-set-key (kbd "<f5>") 'revert-buffer)        ; revert buffer from file
(global-set-key (kbd "<f8>") 'deft)                 ; deft mode
(global-set-key (kbd "M-<f11>") 'menu-bar-mode)		; toggle menubar

(global-set-key (kbd "M-<f12>") 'tabbar-mode)		; toggle tabbar

;; Multiple cursurs default key bindings
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Tabbar mode navigation
;; (global-set-key (kbd "M-<left>") 'tabbar-backward-tab)
;; (global-set-key (kbd "M-<right>") 'tabbar-forward-tab)

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; (global-unset-key (kbd "C-z"))				; disable suspend

;; http://stackoverflow.com/questions/879011/is-it-possible-to-change-emacs-regexp-syntax?lq=1
;; https://github.com/benma/visual-regexp-steroids.el
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

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

(defun .emacs () (interactive) (find-file "~/.emacs"))

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

;;
;; Dired

;; default dired parameters
(setq dired-listing-switches "-tl --group-directories-first --no-group")
(add-hook 'dired-mode-hook (lambda ()
                             (local-set-key (kbd "C-c C-c") 'wdired-change-to-wdired-mode)
                             ))


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
;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:setup-keys t)                      ; optional
;(setq jedi:complete-on-dot t)                 ; optional

;(add-hook 'python-mode-hook 'jedi:ac-setup)

;; Markdown/R-files R.md
(defun markdown-r-keys ()
  "my keys for `Markdown/R mode'."
  (interactive)
  (local-set-key (kbd "C-c C-e") 'ess-eval-paragraph)
  )
(add-hook 'markdown-mode-hook 'markdown-r-keys)

;; ORG MODE
;; needs org-mode checked out in ~/git
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

;; TRAMP MODE
(require 'tramp)
(tramp-cleanup-all-connections)
(setq tramp-default-method "ssh")
(defalias 'tc 'tramp-cleanup-all-connections)
;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

;; DEFT MODE
;; https://github.com/jrblevin/deft
(require 'deft)
(setq deft-extensions '("txt" "org" "md"))
(setq deft-directory "~/git/notes")
(setq deft-use-filename-as-title t)

;; setup files ending in md to markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://bzg.fr/emacs-strip-tease.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears/eyes
(setq visible-bell 0)


(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; Activate hidden-mode-line-mode
;(hidden-mode-line-mode 1)

;; If you want to hide the mode-line in all new buffers
;(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; Alternatively, you can paint your mode-line in White but then
;; you'll have to manually paint it in black again
;; (custom-set-faces
;;  '(mode-line-highlight ((t nil)))
;;  '(mode-line ((t (:foreground "white" :background "white"))))
;;  '(mode-line-inactive ((t (:background "white" :foreground "white")))))

;; A small minor mode to use a big fringe
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; Now activate this global minor mode
;; (bzg-big-fringe-mode 1)

;; To activate the fringe by default and deactivate it when windows
;; are split vertically, uncomment this:
;; (add-hook 'window-configuration-change-hook
;;           (lambda ()
;;             (if (delq nil
;;                       (let ((fw (frame-width)))
;;                         (mapcar (lambda(w) (< (window-width w) fw))
;;                                 (window-list))))
;;                 (bzg-big-fringe-mode 0)
;;               (bzg-big-fringe-mode 1))))

;; Use a minimal cursor
;; (setq default-cursor-type 'hbar)

;; Get rid of the indicators in the fringe
(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
        fringe-bitmaps)

;;
;; Projectile configuration
;; https://github.com/bbatsov/projectile
;;

(projectile-global-mode) ; always run projectil
(setq projectile-enable-caching t) ; cache project files


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote ("	" "	")))
 '(inhibit-startup-screen t)
 '(lua-indent-level 2)
 '(markdown-css-path "http://jasonm23.github.io/markdown-css-themes/foghorn.css")
 '(markdown-xhtml-header-content "<style> p { text-align:
 justify; } </style>")
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (defun snowth-deploy nil
             (interactive)
             (message
              (concat "Running deploying on " project-root))
             (shell-command
              (concat project-root "deploy.sh")))
           (defun snowth-test nil
             (interactive)
             (message
              (concat "Running test on " project-root))
             (shell-command
              (concat project-root "lua/test/runTest.sh")))
           (local-set-key
            (kbd "C-c C-c")
            (quote snowth-test))
           (local-set-key
            (kbd "C-c C-d")
            (quote snowth-deploy))))))
 '(todotxt-file "/home/hartmann/Dropbox/todo/todo.txt" nil (todotxt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face))) t)
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face))) t)
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face))) t)
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face))) t)
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face))) t)
 '(font-latex-subscript-face ((t nil)) t)
 '(font-latex-superscript-face ((t nil)) t)
 '(magit-diff-added ((t (:background "black" :foreground "#ddffdd"))))
 '(magit-diff-added-highlight ((t (:background "blue" :foreground "#cceecc")))))
