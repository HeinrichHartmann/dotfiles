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
