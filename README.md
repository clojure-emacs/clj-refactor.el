# clj-refactor.el [![Build Status](https://secure.travis-ci.org/magnars/clj-refactor.el.png)](http://travis-ci.org/magnars/clj-refactor.el)

A tiny collection of clojure refactoring functions. Please send help.

## Installation

I highly recommend installing clj-refactor through elpa.

It's available on [marmalade](http://marmalade-repo.org/) and
[melpa](http://melpa.milkbox.net/):

    M-x package-install clj-refactor

You can also install the dependencies on your own, and just dump
clj-refactor in your path somewhere:

 - <a href="https://github.com/magnars/s.el">s.el</a>
 - <a href="https://github.com/magnars/dash.el">dash.el</a>
 - <a href="https://github.com/capitaomorte/yasnippet">yasnippet</a>
 - <a href="http://mumble.net/~campbell/emacs/paredit.el">paredit</a>
 - <a href="https://github.com/magnars/multiple-cursors.el">multiple-cursors</a>

## Setup

```cl
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               ))
```

You'll also have to set up the keybindings in the lambda. Read on.

## Setup keybindings

All functions in clj-refactor have a two-letter mnemonic shortcut. For
instance, rename-file is `rf`. You get to choose how those are bound.
Here's how:

```cl
(cljr-add-keybindings-with-prefix "C-c C-m")
;; eg. rename files with `C-c C-m rf`.
```

If you would rather have a modifier key, instead of a prefix, do:

```cl
(cljr-add-keybindings-with-modifier "C-s-")
;; eg. rename files with `C-s-r C-s-f`.
```

If neither of these appeal to your sense of keyboard layout aesthetics, feel free
to pick and choose your own keybindings with a smattering of:

```cl
(define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
```

## Usage

This is it so far:

 - `th`: thread another expression
 - `uw`: unwind a threaded expression
 - `il`: introduce let
 - `el`: expand let
 - `rf`: rename file, update ns-declaration, and then query-replace new ns in project.
 - `ar`: add require to namespace declaration, then jump back (see optional setup)
 - `au`: add "use" (ie require refer all) to namespace declaration, then jump back
 - `ai`: add import to namespace declaration, then jump back

Combine with your keybinding prefix/modifier.

## Thread / unwind example

Given this:

```cl
(map square (filter even? [1 2 3 4 5]))
```

Start by wrapping it in a threading macro:

```cl
(->> (map square (filter even? [1 2 3 4 5])))
```

And start threading away, using `cljr-thread`:

```cl
(->> (filter even? [1 2 3 4 5])
     (map square))
```

And again:

```cl
(->> [1 2 3 4 5]
     (filter even?)
     (map square))
```

To revert this, there's `cljr-unwind`. Just read the examples in the
other direction.

## Introduce / expand let example

Given this:

```cl
(defn handle-request
  {:status 200
   :body (find-body abc)})
```

With the cursor in front of `(find-body abc)`, I do `cljr-introduce-let`:

```cl
(defn handle-request
  {:status 200
   :body (let [X (find-body abc)]
           X)})
```

Now I have two cursors where the `X`es are. Just type out the name,
and press enter. Of course, that's not where I wanted the let
statement. So I do `cljr-expand-let`:

```cl
(defn handle-request
  (let [body (find-body abc)]
    {:status 200
     :body body}))
```

Yay.

## Optional setup

If you're not using yasnippet, then the "jumping back"-part of adding to
namespace won't work. To remedy that, enable the mode with either:

```cl
(yas/global-mode 1)
```

or

```cl
(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))
```

It is an excellent package, so I recommend looking into it. Here are
some snippet packages for Clojure:

 - David Nolen has created some [clojure-snippets](https://github.com/swannodette/clojure-snippets)
 - I've made some [datomic-snippets](https://github.com/magnars/datomic-snippets)

## Automatic insertion of namespace declaration

When you open a blank `.clj`-file, clj-refactor inserts the namespace
declaration for you.

It will also add the relevant `:use` clauses in test files, normally
using `clojure.test`, but if you're depending on midje in your
`project.clj` it uses that instead.

Like clojure-mode, clj-refactor presumes that you are postfixing your
test files with `_test`.

Prefer to insert your own ns-declarations? Then:

```cl
(setq clj-add-ns-to-blank-clj-files nil)
```

## Contribute

Yes, please do. There's a suite of tests, so remember to add tests for your
specific feature, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/clj-refactor.el

To fetch the test dependencies, install
[cask](https://github.com/rejeep/cask.el) if you haven't already,
then:

    $ cd /path/to/clj-refactor
    $ cask

Run the tests with:

    $ ./run-tests.sh

## Contributors

- [AlexBaranosky](https://github.com/AlexBaranosky) added support for `some->` in thread and unwind.

Thanks!

## License

Copyright © 2012-2013 Magnar Sveen

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
