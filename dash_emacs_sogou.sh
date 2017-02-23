#!/bin/bash


if [ ! -x /usr/bin/emacs24-x_original ]
then
sudo mv /usr/bin/emacs24-x /usr/bin/emacs24-x_original
echo "LC_CTYPE='zh_CN.UTF-8' emacs24-x_original" | sudo tee /usr/bin/emacs24-x
sudo chmod a+x /usr/bin/emacs24-x
fi
