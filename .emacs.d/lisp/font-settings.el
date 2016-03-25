;; Cycle font script
;; List of fonts to cycle through
(defun cycle-font ()
  "Change font in current frame.
When called repeatedly, cycle thru a predefined set of fonts."
  (interactive)

  (let (fontList fontToUse currentState)
    ;; states starts from 1.
    (setq fontList (list
                    "Hack" "Ubuntu Mono" "DejaVu Serif" "DejaVu Sans" "DejaVu Sans Mono"
                    "Erica Type :spacing monospace" "Traveling_Typewriter"
                    ))
    (setq currentState (if (get this-command 'state) (get this-command 'state) 1))
    (setq fontToUse (nth (1- currentState) fontList))
    (set-frame-parameter nil 'font fontToUse)
    (message "Current font is: %s" fontToUse)
    (put this-command 'state (1+ (% currentState (length fontList))))
    (redraw-frame (selected-frame))
    )
  )
