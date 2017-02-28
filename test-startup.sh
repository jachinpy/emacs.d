#!/bin/sh -e
if [ -n "$TRAVIS" ]; then
    # Make it look like this is ~/.emacs.d (needed for Emacs 24.3, at least)
    export HOME=$PWD/..
    ln -s emacs.d ../.emacs.d
fi
echo "Attempting startup..."

if [ -x /usr/bin/emacs24-x_original ]
then
   EMACS_BIN=/usr/bin/emacs24-x_original
else
   EMACS_BIN=emacs
fi

${EMACS:=$EMACS_BIN} -nw --batch \
                --eval '(let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init.el"))
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook)))'
echo "Startup successful"
