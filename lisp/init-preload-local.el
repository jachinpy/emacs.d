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

(require-package 'yasnippet)
(require-package 'evil)
(require-package 'w3m)
(require-package 'helm)
(require-package 'projectile)

(eval-after-load 'projectile
  '(progn
     (require-package 'helm-projectile)
     (require 'helm-projectile)
     (require-package 'helm-descbinds))
  )

(require-package 'ensime)
(require-package 'jedi)
(require-package 'elpy)
(require-package 'anaconda-mode)
(require-package 'web-mode)
(require-package 'nyan-mode)

(provide 'init-preload-local)
