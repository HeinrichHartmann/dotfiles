;; TRAMP MODE
(require 'tramp)

(setq tramp-default-method "ssh")
(defalias 'tc 'tramp-cleanup-all-connections)
