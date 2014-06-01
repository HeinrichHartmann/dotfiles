;; Cycle font script
;; List of fonts to cycle through
;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;; '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "unknown" :family "DejaVu Sans")))))

;;     (setq fontList (list "Courier New-10" "Arial Unicode MS-10"
;;                         "Unifont-12" "FixedsysTTF-11" "Code2000-11" "Lucida Sans Unicode-10"))

;; Courier New-12
(defun cycle-font ()
  "Change font in current frame.
When called repeatedly, cycle thru a predefined set of fonts.
Warning: tested on Windows Vista only."
  (interactive)

  (let (fontList fontToUse currentState)
    ;; states starts from 1.
    (setq fontList (list "DejaVu Sans" "DejaVu Sans Mono"))
    (setq currentState (if (get this-command 'state) (get this-command 'state) 1))
    (setq fontToUse (nth (1- currentState) fontList))

    (set-frame-parameter nil 'font fontToUse)
    (message "Current font is: %s" fontToUse)
    (put this-command 'state (1+ (% currentState (length fontList))))
    (redraw-frame (selected-frame))
    )
  )

