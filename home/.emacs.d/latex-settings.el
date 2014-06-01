;; Latex Einstellungen
(add-hook 'LaTeX-mode-hook 'flyspell-mode)	; Mark spelling errors
(add-hook 'LaTeX-mode-hook 'tex-pdf-mode)	; Always produce pdfs
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

(setq TeX-auto-save 1)
(setq TeX-parse-self 1)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)	; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)	; with Emacs latex mode

;; (add-hook 'latex-mode-hook '(local-set-key (kbd "C-\\") 'TeX-complete-symbol))

(global-set-key (kbd "C-\\") 'TeX-complete-symbol)
(global-set-key (kbd "C-c s") 'reftex-search-document)	         	