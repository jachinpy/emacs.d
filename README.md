[![Build Status](https://travis-ci.org/jachinpy/emacs.d.png?branch=develop)](https://travis-ci.org/jachinpy/emacs.d)
# A reasonable Emacs config

Emacs itself comes with support for many programming languages. This
config adds improved defaults and extended support for the following:

* Ruby / Ruby on Rails
* CSS / LESS / SASS / SCSS
* HAML / Markdown / Textile / ERB
* Clojure (with Cider and nRepl)
* Javascript / Coffeescript
* Python
* PHP
* Haskell
* Erlang
* Common Lisp (with Slime)

In particular, there's a nice config for *tab autocompletion*, and
`flycheck` is used to immediately highlight syntax errors in Ruby, HAML,
Python, Javascript, PHP and a number of other languages.

## Supported Emacs versions

The config should run on Emacs 23.3 or greater and is designed to
degrade smoothly - see the Travis build - but note that Emacs 24 and
above is required for an increasing number of key packages, including
`magit` and `flycheck`, so to get full you should use the latest Emacs
version available to you.

## Other requirements

To make the most of the programming language-specific support in this
config, further programs will likely be required, particularly those
that [flycheck](https://github.com/flycheck/flycheck) uses to provide
on-the-fly syntax checking.
To make the most of the programming language-specific support in this
config, further programs will likely be required, particularly those
that [flycheck](https://github.com/flycheck/flycheck) uses to provide
on-the-fly syntax checking.

## Installation

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the
`init.el` contained in this repo ends up at `~/.emacs.d/init.el`:

```
git clone https://github.com/jachinpy/emacs.d.git ~/.emacs.d
```

Upon starting up Emacs for the first time, further third-party
packages will be automatically downloaded and installed. If you
encounter any errors at that stage, try restarting Emacs, and possibly
running `M-x package-refresh-contents` before doing so.



## Important note about `ido`

This config enables `ido-mode` completion in the minibuffer wherever
possible, which might confuse you when trying to open files using
<kbd>C-x C-f</kbd>, e.g. when you want to open a directory to use
`dired` -- if you get stuck, use <kbd>C-f</kbd> to drop into the
regular `find-file` prompt. (You might want to customize the
`ido-show-dot-for-dired` variable if this is an issue for you.)

## Updates

Update the config with `git pull`. You'll probably also want/need to update
the third-party packages regularly too:

<kbd>M-x package-list-packages</kbd>, then <kbd>U</kbd> followed by <kbd>x</kbd>.

## Adding your own customization

To add your own customization, use <kbd>M-x customize</kbd> and/or
create a file `~/.emacs.d/lisp/init-local.el` which looks like this:

```el
... your code here ...

(provide 'init-local)
```

If you need initialisation code which executes earlier in the startup process,
you can also create an `~/.emacs.d/lisp/init-preload-local.el` file.

If you plan to customize things more extensively, you should probably
just fork the repo and hack away at the config to make it your own!


## feature

 + add vim style key "%" replace C-M-n or C-M-p
 + add evil-mode, yas-global-mode
 + remove menu.
 + add root edit current file.
 + fix fill colum bug.
 + add w3m
 + add dired mode create file.
 + add helm, helm-projectile, helm-descbinds-mode
 + add mew
 + add ensime
 + change backup dir in /tmp/{hostname}/
 + add python IDE,pymacs, ropemacs, rope
 + add web-mode config.
 + add projectile.
 + add fullscreen and key.
 + add function my-delete-leading-whitespace.
 + add more packages, eg: elpy, pymacs, and so on.
