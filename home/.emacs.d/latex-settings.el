;; AUCTex Latex mode settings
(add-hook 'LaTeX-mode-hook 'flyspell-mode)	; Mark spelling errors
(add-hook 'LaTeX-mode-hook 'tex-pdf-mode)	; Always produce pdfs
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)	; with AUCTeX LaTeX mode

;; Emacs Latex mode settings

(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-buffer)
(add-hook 'latex-mode-hook 'visual-line-mode)
(add-hook 'latex-mode-hook (lambda() (set-visual-wrap-column 100)))
(add-hook 'latex-mode-hook 'turn-on-reftex)	; with Emacs latex mod

; (add-hook 'latex-mode-hook '(local-set-key (kbd "C-\\") 'TeX-complete-symbol))
; (add-hook 'latex-mode-hook '(local-set-key (kbd "C-c s") 'reftex-search-document))

(setq TeX-auto-save 1)
(setq TeX-parse-self 1)
(setq-default TeX-master nil)
;; (add-hook 'latex-mode-hook '(local-set-key (kbd "C-c w") 'writing-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun writing-mode ()
  (interactive)
  (setq fill-column 120)
  (load-theme 'tango)
  (center-window)
  )

(defun center-window ()
  (interactive)
  (let ((margin (- (/ (- (window-body-width) fill-column) 2) 10)))
    (set-fringe-style `(5 . 5))
    (set-window-margins nil margin margin)
    )
  )

(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column) (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
	(when (window-live-p (car windows))
	  (with-selected-window (car windows)
	    (update-visual-wrap-column)))
	(setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
	   (right-margin (or (cdr current-margins) 0))
	   (current-width (window-width))
	   (current-available (+ current-width right-margin))
	   )
      (if (<= current-available visual-wrap-column)
	  (set-window-margins nil (car current-margins))
	(set-window-margins nil (car current-margins) (- current-available visual-wrap-column)))
      )
    )
  )
