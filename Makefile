.PHONY: clean compile lint test
.DEFAULT_GOAL := all

# Something like this can be handy if you need Eldev to run on an Emacs other than your default one:
# export ELDEV_EMACS="$HOME/emacs28/Emacs.app/Contents/MacOS/Emacs"

clean:
	echo

# You can find a generic `eldev` installation script in https://github.com/emacs-eldev/eldev/blob/master/webinstall/eldev
# (Don't use the one defined for CircleCI in your local machine)

lint: clean
	eldev lint

# Checks for byte-compilation warnings.
compile: clean
	 eldev -dtT compile --warnings-as-errors

test: clean
	eldev -dtT -p test

all: lint compile test
