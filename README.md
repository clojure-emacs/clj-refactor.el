[![MELPA](http://melpa.org/packages/clj-refactor-badge.svg)](http://melpa.org/#/clj-refactor)
[![MELPA Stable](http://stable.melpa.org/packages/clj-refactor-badge.svg)](http://stable.melpa.org/#/clj-refactor)
[![Build Status](https://secure.travis-ci.org/clojure-emacs/clj-refactor.el.png)](http://travis-ci.org/clojure-emacs/clj-refactor.el)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/clojure-emacs/refactor-nrepl?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

# clj-refactor.el

`clj-refactor` provides refactoring support for clojure projects.

## Installation

I highly recommend installing clj-refactor through elpa.

It's available on [marmalade](http://marmalade-repo.org/) and
[melpa](http://melpa.org/):

    M-x package-install clj-refactor

## Setup

```el
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               ))
```

You'll also have to set up the keybindings in the lambda. Read on.

### Setup keybindings

All functions in clj-refactor have a two-letter mnemonic shortcut. For
instance, rename-file-or-dir is `rf`. You get to choose how those are bound.
Here's how:

```el
(cljr-add-keybindings-with-prefix "C-c C-m")
;; eg. rename files with `C-c C-m rf`.
```

If you would rather have a modifier key, instead of a prefix, do:

```el
(cljr-add-keybindings-with-modifier "C-s-")
;; eg. rename files with `C-s-r C-s-f`.
```

If neither of these appeal to your sense of keyboard layout aesthetics, feel free
to pick and choose your own keybindings with a smattering of:

```el
(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file-or-dir)
```

**The keybindings suggested here might be conflicting with keybindings in
either clojure-mode or cider. Ideally, you should pick keybindings that don't
interfere with either.**

### Optional setup

#### Refactor nREPL middleware

For some of the more advanced refactorings we've written an [nrepl](https://github.com/clojure/tools.nrepl) middleware called
[refactor-nrepl](https://github.com/clojure-emacs/refactor-nrepl).

Add the following, either in your project's `project.clj` or in the `:user`
profile found at `~/.lein/profiles.clj`:

```clojure
:plugins [[refactor-nrepl "1.0.5"]]
```

**WARNING** The analyzer needs to eval the code too in order to be able to build
  the AST we can work with. If that causes side effects like writing files,
  opening connections to servers, modifying databases, etc. performing certain
  refactoring functions on your code will do that, too.

#### Yasnippet
If you're not using yasnippet, then the "jumping back"-part of adding to
namespace won't work. To remedy that, enable the mode with either:

```el
(yas/global-mode 1)
```

or

```el
(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))
```

It is an excellent package, so I recommend looking into it.

## Usage

See the wiki for a complete [list of available refactorings] (https://github.com/clojure-emacs/clj-refactor.el/wiki), demonstrations and customization points.

## Changelog

An extensive changelog is available [here](CHANGELOG.md).

## Contribute

Yes, please do. There's a suite of tests, so remember to add tests for your
specific feature, or I might break it later.

You'll find the repo at:

    https://github.com/clojure-emacs/clj-refactor.el

To fetch the test dependencies, install
[cask](https://github.com/cask/cask) if you haven't already,
then:

    $ cd /path/to/clj-refactor
    $ cask

Run the tests with:

    $ ./run-tests.sh


Before submitting a patch or a pull request make sure all tests are
passing and that your patch is in line with the [contribution
guidelines](CONTRIBUTING.md).

## Contributors

The project is maintained by

- [Magnar Sveen](https://github.com/magnars)
- [Lars Andersen](https://github.com/expez)
- [Benedek Fazekas](https://github.com/benedekfazekas)
- [Alex Baranosky](https://github.com/AlexBaranosky)

Additional contributors are:

- [Bozhidar Batsov](https://github.com/bbatsov)
- [Ryan Smith](https://github.com/tanzoniteblack)
- [Hugo Duncan](https://github.com/hugoduncan)
- [Luke Snape](https://github.com/lsnape)
- [Aaron France](https://github.com/AeroNotix)

Thanks!

## License

Copyright © 2012-2015 Magnar Sveen

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
