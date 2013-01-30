# clj-refactor.el

A tiny, but growing, collection of clojure refactoring functions.

## Installation

It is highly recommended to install it through elpa.

It's available on [marmalade](http://marmalade-repo.org/):

    M-x package-install clj-refactor

You can also install the dependencies on your own, and just dump
clj-refactor in your path somewhere:

 - <a href="https://github.com/magnars/s.el">s.el</a>
 - <a href="https://github.com/magnars/dash.el">dash.el</a>


## Setup

    (require 'clj-refactor)
    (add-hook 'clojure-mode-hook (lambda ()
                                   (clj-refactor-mode 1)
                                   ;; insert keybinding setup here
                                   ))

You'll also have to set up the keybindings in the lambda. Read on.

## Setup keybindings

All functions in clj-refactor have a two-letter mnemonic shortcut. For
instance, rename-file is `rf`. You get to choose how those are bound.
Here's how:

    (cljr-add-keybindings-with-prefix "C-c C-m")
    ;; eg. rename files with `C-c C-m rf`.

If you would rather have a modifier key, instead of a prefix, do:

    (cljr-add-keybindings-with-modifier "C-s-")
    ;; eg. rename files with `C-s-r C-s-f`.

If neither of these appeal to your sense of keyboard layout aesthetics, feel free
to pick and choose your own keybindings with a smattering of:

    (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)

## Use

This is it so far:

 - `rf`: rename file, update ns-declaration, and then query-replace new ns in project.
 - `ar`: add :require to namespace declaration
 - `au`: add :use to namespace declaration

Combine with your keybinding prefix/modifier.

## License

Copyright (C) 2012 Magnar Sveen

Authors: Magnar Sveen <magnars@gmail.com>
Keywords: clojure convenience

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
