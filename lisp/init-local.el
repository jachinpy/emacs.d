;;; Code

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
(yas-global-mode 1)
;;; keeping YASnippet defaults try out ~/Downloads/interesting-snippets
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets/django")
                               '("~/.emacs.d/yasnippet-snippets")))

(evil-mode 1)
(setq evil-default-state 'emacs)
(define-key evil-emacs-state-map (kbd "C-o") 'evil-execute-in-normal-state)

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

(setq-default initial-scratch-message
              (concat ";; Welcome " (or user-login-name "")
                      "!\n;; Experience must be bought.\n\n"))

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

(package-initialize)
(elpy-enable)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

(add-to-list 'load-path "~/.emacs.d/pymacs")
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-guess-project t)
  (setq ropemacs-autoimport-modules
        '("argparse" "bisect" "calendar" "collections" "ConfigParser" "datetime" "distutils" "errno" "exceptions" "fileinput" "fnmatch" "formatter" "fractions" "functools" "getopt" "glob" "hashlib" "heapq" "io" "itertools" "json" "logging" "math" "mimetypes" "os" "os.path" "pickle" "pickletools" "pipes" "platform" "pprint" "pydoc" "pyqcy" "random" "re" "repr" "setuptools" "shutil" "string" "sys" "tempfile" "time" "timeit" "urllib" "urllib2" "urlparse" "uuid" "weakref"))
  )
(global-set-key "\C-xpl" 'load-ropemacs)

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

(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun edit-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

(autoload 'auto-revert-mode "autorevert" 0 t)
(autoload 'turn-on-auto-revert-mode "autorevert" 0 0)
(autoload 'global-auto-revert-mode "autorevert" 0 t)
(global-auto-revert-mode 1)

(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
    (if (not (bolp)) (forward-line 1))
    (delete-whitespace-rectangle (point) end 0)))

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

(defun insert-datetime ()
  "Insert now datetime at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%s %H:%M:%S")))

(setq org-agenda-files (list "~/project/carry/strategy.org"
                             "~/project/carry/plan.org"))

(define-coding-system-alias 'UTF-8 'utf-8)

;; must be sudo apt-get install w3m in ubuntu.
(setq get-w3m-path (file-expand-wildcards "~/.emacs.d/elpa/w3m*"))
(add-to-list 'load-path (car get-w3m-path))
(require 'w3m)
(setq w3m-use-cookies t)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point)

(provide 'init-local)
