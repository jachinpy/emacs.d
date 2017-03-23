;;; package --- china pyim;
;;; Commentary:
;;; Code:
;;; console and cygwin env chinese-input method support.
(require 'chinese-pyim)
(setq default-input-method "chinese-pyim")
(global-set-key (kbd "C-<SPC>") 'toggle-input-method)
(global-set-key (kbd "C-;") 'pyim-toggle-full-width-punctuation)
(setq pyim-use-tooltip t)
(setq pyim-dicts '((:name "BigDict"
                          :file "~/.emacs.d/pyim-bigdict.pyim"
                          :coding utf-8-unix)))
(pyim-restart-1 t)
(provide 'init-pyim)
;;; init-pyim ends here
