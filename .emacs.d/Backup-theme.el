(deftheme Backup
  "Created 2015-12-19.")

(custom-theme-set-variables
 'Backup
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((eval progn (require (quote projectile)) (defun snowth-deploy nil (interactive) (message (concat "Running deploying on " project-root)) (shell-command (concat project-root "deploy.sh"))) (defun snowth-test nil (interactive) (message (concat "Running test on " project-root)) (shell-command (concat project-root "lua/test/runTest.sh"))) (local-set-key (kbd "C-c C-c") (quote snowth-test)) (local-set-key (kbd "C-c C-d") (quote snowth-deploy))))))
 '(todotxt-file "/home/hartmann/Dropbox/todo/todo.txt"))

(custom-theme-set-faces
 'Backup
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face))))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil))))

(provide-theme 'Backup)
