;; ORG MODE
;; needs org-mode checked out in ~/git
;; org mode
(if (file-exists-p "~/git/orgmode/")
    (progn
      (add-to-list 'load-path "~/git/orgmode/org-reveal")
      (add-to-list 'load-path "~/git/orgmode/org-mode/lisp")
      (add-to-list 'load-path "~/git/orgmode/org-mode/contrib/lisp" t)
      (require 'ox-beamer)
      (require 'ox-md)
      (require 'ox-deck)
      (require 'ox-reveal)
      (require 'ox-taskjuggler)
      )
  )

;; Get rid of the indicators in the fringe
(mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
        fringe-bitmaps)


(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
   same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat
           (replace-regexp-in-string "[.]org$"
                                     ""
                                     (file-truename buffer-file-name))
           "_"
           (format-time-string "%Y-%m-%dT%H%M%S_")))
         ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'org-metaup)
            (local-set-key (kbd "M-n") 'org-metadown)
            ;(local-set-key (kbd "C-c C-x I") org-cliplink)
            ;(local-set-key (kbd "C-c C-x S") org-screenshot)
            ))

; Some initial langauges we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (ditaa . t)
   (dot . t)
   (awk . t)
   (lisp . t)
   ))

(setq org-babel-sh-command "bash")
