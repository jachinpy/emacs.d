;;; Loading helm, use helm-projectile.

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

(projectile-global-mode)
(setq projectile-completion-system 'helm)

(helm-projectile-on)
(helm-descbinds-mode 1)

(global-set-key (kbd "M-t") 'projectile-find-file)

(provide 'init-helm)
