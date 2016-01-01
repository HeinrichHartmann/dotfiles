;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load External Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl) ;; common lisp

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "helpers")          ; convenicence functions
(load "setup-package")    ; setup package manager
(load "setup-el-get")     ; setup el-get
(load "tmux")
(load "font-settings")	  ; default fonts and font cycle script
(load "latex-settings")	  ; add hooks for latex
(load "orgmode-settings")	  ; default fonts and font cycle script
(load "emacs-strip-tease")
(load "jedi-settings")
(load "tramp-settings")

;; install default set of packages
(package-ensure-installed
 'jedi
 'markdown-mode
 'multiple-cursors
 'magit
 'visual-regexp-steroids
 'tabbar
 'projectile
 'page-break-lines
 )

(el-get-ensure-installed
 ;;
)

(require 'multiple-cursors)
(require 'tabbar)
(require 'visual-regexp-steroids)
(require 'epa-file)
(require 'dired-x)
(require 'page-break-lines)
;;(require 'projectile)

;; Render page-break control character (^L) as horizontal line
(global-page-break-lines-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default major-mode 'text-mode)          ; edit files in text mode per default
(menu-bar-mode -1)                            ; show the menu...
(tool-bar-mode -1)                            ; ... but not the the toolbar
(ruler-mode -1)                               ; kein Lineal
(tabbar-mode -1)                              ; Tabbar mode
(icomplete-mode t)                            ; Show auto completin in minibuffer menus
(setq icomplete-prospects-height 1)           ; ...only one line
(scroll-bar-mode -1)                          ; show no scrollbar...
(scroll-bar-mode 'right)                      ; ... on the right
(setq scroll-margin 1                         ; do smooth scrolling, ...
      scroll-conservatively 100000            ; ... the defaults ...
      scroll-up-aggressively 0.01             ; ... are very ...
      scroll-down-aggressively 0.01)          ; ... annoying
(when (fboundp 'set-fringe-mode)              ; emacs22+
  (set-fringe-mode 2))                        ; space left of col1 in pixels
(transient-mark-mode t)                       ; make the current 'selection' visible
(delete-selection-mode -1)                    ; do not delete the selection with a keypress
(setq x-select-enable-clipboard t             ; copy-paste should work ...
      interprogram-paste-function             ; ...with...
      'x-cut-buffer-or-selection-value)       ; ...other X clients

;; Work with visible lines not with (wrapped,) logical lines
(visual-line-mode 1)                          ; 1 for on, 0 for off.
(setq fill-column 80)                         ; Break lines at colum x
(set-face-attribute 'default nil :height 120) ; set default font size
(setq-default show-trailing-whitespace 1)     ; mark whitespace at the end of the line
(defun toggle-show-trailing-whitespace ()
  (interactive)
  (if show-trailing-whitespace
      (setq show-trailing-whitespace nil)
    (setq show-trailing-whitespace t)
    ))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(savehist-mode 1)                             ; keep M-x History

(setq set-mark-command-repeat-pop 1)          ; cycle through marks with a single C-SPC, after the first C-u C-SPC was used.

;; show full directoy in frame title
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

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; don't use arrow keys for cursor movement, but for resizing windows
(global-set-key (kbd "<left>") 'shrink-window-horizontally)
(global-set-key (kbd "<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<down>") 'shrink-window)
(global-set-key (kbd "<up>") 'enlarge-window)

;; switch windows using <s-{arrow keys}>
(windmove-default-keybindings)

;; Cycle through bufers with M-{arrow keys}
(global-set-key (kbd "<M-left>") 'next-buffer)
(global-set-key (kbd "<M-right>") 'previous-buffer)

(global-set-key (kbd "C-x !") 'shell)                 ; a shell
(global-set-key (kbd "C-x $") 'ispell-buffer)         ; spell check
(global-set-key (kbd "C-x c") 'calendar)              ; show calendar
(global-set-key (kbd "C-\\") 'dabbrev-expand)         ; auto complete? (=M-/)

(global-set-key (kbd "C-x w") 'toggle-show-trailing-whitespace)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines) ; warp long lines
(global-set-key (kbd "C-x a") 'align-regexp)          ; align
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; FUNCTION KEYS
(global-set-key (kbd "<f5>") 'revert-buffer)        ; revert buffer from file
(global-set-key (kbd "<f9>") 'cycle-font)	        ; cf . emacs.d/font-settings.el
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)		; toggle menubar; f10 to access menubar.
(global-set-key (kbd "M-<f10>") 'tabbar-mode)		; toggle tabbar
(global-set-key (kbd "<f11>") 'toggle-fullscreen)   ; toggle fullscreen
(global-set-key (kbd "C-<f11>") 'zen-mode)          ; big-fringes, in center (gtk-only)

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; (global-unset-key (kbd "C-z"))				; disable suspend

;; VISUAL REGEXP STEROIDS
;; http://stackoverflow.com/questions/879011/is-it-possible-to-change-emacs-regexp-syntax?lq=1
;; https://github.com/benma/visual-regexp-steroids.el
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

;; convert selection to multiple cursors
(define-key global-map (kbd "C-S-q") 'vr/mc-mark)

;; MULTIPLE CURSORS
;; https://github.com/magnars/multiple-cursors.el
(global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

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


;; DIRED
;; default dired parameters
(setq dired-listing-switches "-tl --group-directories-first --no-group")

;; Omit hidden files in dired mode
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

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)
            (local-set-key (kbd "C-c C-c") 'wdired-change-to-wdired-mode)
            (local-set-key (kbd "u") 'dired-up-directory)
            ))


(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))

;; Encrytion settings
(epa-file-enable) ;; auto encrypt gpg files

;; DGDB MODE
(setq gdb-show-main 1)

;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

;; MARKDOWN MODE
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(eval-after-load 'markdown-mode
    '(progn
       (define-key markdown-mode-map (kbd "M-n") nil)
       (define-key markdown-mode-map (kbd "M-p") nil)
       ))

;; Projectile configuration
;; https://github.com/bbatsov/projectile
;;
;(projectile-global-mode)                ; always run projectil
;(setq projectile-enable-caching t)      ; cache project files

;; MARKING
;; cf. https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(require 'show-marks)
(global-set-key (kbd "<C-right>") 'forward-mark)
(global-set-key (kbd "<C-left>") 'backward-mark)
(global-set-key (kbd "<C-down>") 'show-marks)

;; TODO TXT MODE

;; (require 'todotxt)
;; (define-key todotxt-mode-map (kbd "d") 'todotxt-complete-toggle)
;; (defun todo ()
;;   (interactive)
;;   (let ((buffer-name (find-file-noselect "~/Dropbox/todo/todo.txt")))
;;     (message "buffer-name")
;;     (display-buffer buffer-name)
;;     (switch-to-buffer-other-window buffer-name)
;;     (todotxt-mode)
;;     (end-of-buffer)
;;     )
;;   )
;; (global-set-key (kbd "<f12>") 'todo)

;; (add-hook 'todotxt-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "d") 'todotxt-complete-toggle)
;;             )
;;           )

;; turn of electric indent for lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (message "eim off")
            (electric-indent-mode 0)
            )
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote ("	" "	")))
 '(custom-safe-themes
   (quote
    ("8e73c434ca39176b80c0fec0473813c1b71d8665a4ceb55789c848c3387ef677" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(doc-view-resolution 300)
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
 '(send-mail-function (quote smtpmail-send-it))
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

