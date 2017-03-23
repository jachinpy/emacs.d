;;; package --- elpy, rope ropemacs;
;;; Commentary:
;;; Code:
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

(provide 'init-python-dev)
;;; init-python-dev ends here
