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

(require-package 'evil)
(require-package 'w3m)
(require-package 'ensime)
(require-package 'jedi)
(require-package 'elpy)
(require-package 'anaconda-mode)
(require-package 'web-mode)
(require-package 'nyan-mode)
(require-package 'chinese-pyim)
(require-package 'tabbar)
(require-package 'powerline)
(require-package 'neotree)

(defun get-snippet ()
  (shell-command
   "git clone https://github.com/AndreaCrotti/yasnippet-snippets.git ~/.emacs.d/yasnippet-snippets"))

(if (file-exists-p "~/.emacs.d/yasnippet-snippets")
    (message "Loading ... yasnppet-snippets ok!")
  (get-snippet))

(provide 'init-preload-local)
