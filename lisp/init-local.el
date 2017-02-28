;;; package --- any config；
;;; commentary:

;;; packages:
;;; yasnippet, pymacs, jedi, w3m, virtualenv, slime,
;;; evil, helm, helm-projectile, projectile, helm-descbinds, elpy, ensime,
;;; nyan-mode,...

;;; Debian system depend on packages
;;; sudo apt-get install w3m
;;; sudo pip install rope flake8 pep8 jedi importmagic

;;; python third-packages
;;; git clone http://github.com/pinard/pymacs
;;; cd pymacs
;;; make check
;;; sudo make install
;;; sudo  pip install rope ropemacs

;; (setq debug-on-error t)
;; M-x toggle-debug-on-error
;;; M-x yas-reload-all if you've started YASnippet already.
;;; Code:
(yas-global-mode 1)
;;; keeping YASnippet defaults try out ~/Downloads/interesting-snippets
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets/django")
                               '("~/.emacs.d/yasnippet-snippets")))
;;; fix term can't use <tab> key.
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate 1)))

(setq backup-directory-alist (quote (("." . "~/.backups"))))

;;; set js2-mode 4 space
(setq js2-basic-offset 4)

(evil-mode 1)
(setq evil-default-state 'emacs)
(define-key evil-emacs-state-map (kbd "C-o") 'evil-execute-in-normal-state)

;; powerline state
(require 'evil)
(load-theme 'airline-light)
(setq airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)


;;; Fix bug. fci-rule-color can use in dark color.
;; "lightgreen", "green", "darkblue" ,"light green" and so on.
(make-variable-buffer-local 'line-move-visual)
(defadvice previous-line (around avoid-jumpy-fci activate)
  (if (and (symbol-value 'fci-mode) (> (count-lines 1 (point)) 0))
      (prog (fci-mode -1) ad-do-it (fci-mode 1))
    ad-do-it))
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (fci-mode 1)
                                  ))

(define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
(my-global-fci-mode 1)
(setq-default fill-column 80)
(setq fci-rule-color "light green")

(tabbar-mode 1)

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; Optional setup (Read Mail menu):
(setq read-mail-command 'mew)

;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

(setq mew-use-cached-passwd t)

(require 'ensime) ;;; Read code.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(nyan-mode 1)
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

(global-hl-line-mode 1)

(global-set-key "&" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(autoload 'auto-revert-mode "autorevert" 0 t)
(autoload 'turn-on-auto-revert-mode "autorevert" 0 0)
(autoload 'global-auto-revert-mode "autorevert" 0 t)
(global-auto-revert-mode 1)

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (, tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(setq org-agenda-files (list "~/project/carry/strategy.org"
                             "~/project/carry/plan.org"))

(define-coding-system-alias 'UTF-8 'utf-8)

;; must be sudo apt-get install w3m in ubuntu.
;; (setq get-w3m-path (file-expand-wildcards "~/.emacs.d/elpa/w3m*"))
;; (add-to-list 'load-path (car get-w3m-path))
;; (require 'w3m)
;; (setq w3m-use-cookies t)
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; (global-set-key "\C-xm" 'browse-url-at-point)

;; neotree
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(global-set-key [f8] 'neotree-toggle)

(provide 'init-local)
;;; init-local.el ends here
