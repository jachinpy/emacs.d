;;; code

;;; add packages source.
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;; python env.
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(provide 'init-preload-local)
