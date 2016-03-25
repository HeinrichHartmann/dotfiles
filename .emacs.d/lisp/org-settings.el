;; ORG--MODE

(require 'org-latex)
(require 'org-publish)
(require 'org-jsinfo)
;(require 'org-beamer)

;; Standard settings

(setq org-todo-keywords
      '((sequence "TODO" "WAIT" "|" "DONE" "CANCELED")))


(setq org-export-html-style-default "<link rel=\"stylesheet\" type=\"text/css\"href=\"css/org.css\" />
<script type="text/javascript" src="http://miql.zxq.net/MathJax/MathJax.js"></script>")
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "chromium-browser")

;; RefTex Settings
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)


(unless (boundp 'org-export-latex-classes) (setq org-export-latex-classes nil))


;; Heinrich's own article preamble
(add-to-list 'org-export-latex-classes 
	     '("myart"
	       "\\documentclass[a4paper,11pt]{amsart}
\\include{latex/preamble}
\\usepackage[all,cmtip]{xy}
\\usepackage{graphicx}
\\usepackage{lmodern}
%\\usepackage[latin1]{inputenc}
\\usepackage{verbatim}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	       )
	     )



;; #+LaTeX_CLASS: beamer in org files
(add-to-list 'org-export-latex-classes
	     ;; beamer class, for presentations
	     '("mybeamer"
	       "\\documentclass[11pt]{beamer}\n
\\mode<{{{beamermode}}}>\n
\\usetheme{{{{beamertheme}}}}\n
\\usecolortheme{{{{beamercolortheme}}}}\n
\\beamertemplateballitem\n
\\setbeameroption{show notes}

\\setbeamertemplate{navigation symbols}{}\n
\\useoutertheme{infolines}\n

\\include{latex/preamble}\n
\\usepackage[all,cmtip]{xy}\n

%\\usepackage[latin1]{inputenc}\n
\\usepackage[utf8]{inputenc}\n
\\usepackage[T1]{fontenc}\n
\\usepackage{hyperref}\n
\\usepackage{color}
\\usepackage{listings}
\\lstset{numbers=none,language=[ISO]C++,tabsize=4,
frame=single,
basicstyle=\\small,
showspaces=false,showstringspaces=false,
showtabs=false,
keywordstyle=\\color{blue}\\bfseries,
commentstyle=\\color{red},
}\n
\\usepackage{verbatim}\n
\\institute{{{{beamerinstitute}}}}\n          
\\subject{{{{beamersubject}}}}\n"

	       ("\\section{%s}" . "\\section*{%s}")
   
	       ("\\begin{frame}[fragile]\\frametitle{%s}" "\\end{frame}" 
		"\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}")
	       )
	     )

