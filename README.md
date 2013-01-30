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
    (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))

## Use

Right now, there's only this:

 - `C-x C-r`: rename file, update ns-declaration, and then query-replace new ns in project.

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
