;;; code
;; packages:
;; (neotree, Fill-Column-Indicator, yasnippet, multiple-cursors
;; ido, term, jedi, w3m, virtualenv, slime, multiterm, solarized-theme,
;; )

;;; disable menu
(menu-bar-mode -1)

;;; time
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

;;; yasnippet, M-x yas-reload-all if you've started YASnippet already.
(yas-global-mode 1)
;;; keeping YASnippet defaults try out ~/Downloads/interesting-snippets
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets/django")
                               '("~/.emacs.d/yasnippet-snippets")))

;;; start file as root.
(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

;;; neotree
(global-set-key [f8] 'neotree-toggle)

;;; autorevert stuff
(autoload 'auto-revert-mode "autorevert" 0 t)
(autoload 'turn-on-auto-revert-mode "autorevert" 0 0)
(autoload 'global-auto-revert-mode "autorevert" 0 t)
(global-auto-revert-mode 1)

;;; Fill-Column-Indicator, fix bug.
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

;;; delete white space
(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
    (if (not (bolp)) (forward-line 1))
    (delete-whitespace-rectangle (point) end 0)))

;;; w3m
;;; ubuntu, sudo apt-get install w3m
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

;;; fullscreen
;;; reference: http://www.emacswiki.org/cgi-bin/wiki/FullScreen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(provide 'init-local)
