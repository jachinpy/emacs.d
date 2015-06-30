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
(helm-mode 1)

(eval-after-load "helm-mode"
    '(progn
       (require-package 'helm-projectile)
       (require 'helm-projectile)
       (require-package 'helm-descbinds))
  )

(add-to-list 'load-path "~/.emacs.d/elpa/helm")
(require 'helm)
(require 'helm-config)
(require 'helm-misc)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(require-package 'ensime)
(require-package 'jedi)
(require-package 'elpy)
(require-package 'anaconda-mode)
(require-package 'web-mode)
(require-package 'nyan-mode)

(defun get-snippet ()
  (shell-command
   "git clone https://github.com/AndreaCrotti/yasnippet-snippets.git ~/.emacs.d/yasnippet-snippets"))

(if (file-exists-p "~/.emacs.d/yasnippet-snippets")
    (message "load yasnppet-snippets")
  (get-snippet))

(provide 'init-preload-local)
