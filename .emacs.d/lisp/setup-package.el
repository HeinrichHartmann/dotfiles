;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
                        ;;("marmalade" . "http://marmalade-repo.org/packages/")
			 ))



; activate all the packages (in particular autoloads)
(package-initialize)

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir) (package-refresh-contents))

;; Stolen from: http://stackoverflow.com/questions/10092322/
(defun package-ensure-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)
       )
     )
   packages
   )
  )
