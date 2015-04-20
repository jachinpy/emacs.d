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

;;; hightlight line.
(global-hl-line-mode 1)

;;; yasnippet, M-x yas-reload-all if you've started YASnippet already.
(yas-global-mode 1)
;;; keeping YASnippet defaults try out ~/Downloads/interesting-snippets
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets/django")
                               '("~/.emacs.d/yasnippet-snippets")))

;;; evil-mode for vim
(evil-mode 1)
(setq evil-default-state 'emacs)
(define-key evil-emacs-state-map (kbd "C-o") 'evil-execute-in-normal-state)

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
;; fci-rule-color can use in dark color.
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

;;; Here's an adaptation of dired-create-directory.
;;; It works the same way, so as well as a plain filename,
;;; you can also specify new parent directories
;;;(to be created under the current directory) for the file (e.g. foo/bar/filename).
(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-c n") 'my-dired-create-file)
     (defun my-dired-create-file (file)
       "Create a file called FILE.
If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))

;;; helm and projectil
(add-to-list 'load-path "~/.emacs.d/elpa/helm-20150331.1105")
(require 'helm-config)
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
;;; helm-descbinds
(helm-descbinds-mode 1)

;;; ropemacs
(add-to-list 'load-path "~/.emacs.d/vendor/pinard-Pymacs-e3f67f9")
(require 'pymacs)
;; (let ((process
;;         (apply 'start-process "pymacs" buffer
;;                (let ((python (getenv "PYMACS_PYTHON")))
;;                  (if (or (null python) (equal python ""))
;;                      pymacs-python-command
;;                    python))
;;                "-c" (concat "import sys;"
;;                             " from Pymacs import main;"
;;                             " main(*sys.argv[1:])")
;;                (append
;;                 (and (>= emacs-major-version 24) '("-f"))
;;                 (mapcar 'expand-file-name pymacs-load-path)))))

(eval-after-load 'python
  '(progn
     (autoload 'pymacs-apply "pymacs")
     (autoload 'pymacs-call "pymacs")
     (autoload 'pymacs-eval "pymacs" nil t)
     (autoload 'pymacs-exec "pymacs" nil t)
     (autoload 'pymacs-load "pymacs" nil t)
     (message "loading ropemacs")
     (pymacs-load "ropemacs" "rope-")
     (setq ropemacs-enable-autoimport t)))

(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-ropemacs)))

;;; First scratch message.
(setq-default initial-scratch-message
              (concat ";; Welcome hacking " (or user-login-name "") " ♥ gogogo!\n\n"))

;; mew
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
(provide 'init-local)
