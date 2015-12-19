;;
;; el-get config
;; https://github.com/dimitri/el-get
;;


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))


(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")


;; Stolen from: http://stackoverflow.com/questions/10092322/
(defun el-get-ensure-installed (&rest packages)
  "Assure every package is installed"
  (el-get 'sync packages)
  )
