# nvm.el [![Build Status](https://api.travis-ci.org/rejeep/nvm.el.png?branch=master)](http://travis-ci.org/rejeep/nvm.el)

Manage Node versions within Emacs

## Installation

### Using Cask

Add `nvm` to your [Cask](https://github.com/cask/cask) file:

```lisp
(depends-on "nvm")
```

### Using `use-package` and [straight.el](https://github.com/radian-software/straight.el)

```elisp
(use-package nvm
  :straight (:host github :repo "rejeep/nvm.el")
  :config
  ;; Optionally set a default node version
  (nvm-use "18"))
```

### Using [Quelpa](https://github.com/quelpa/quelpa-use-package)

```elisp
(use-package nvm
  :quelpa ((nvm :fetcher github
                :repo "rejeep/nvm.el")
                :upgrade t)
```

## DSL

### nvm-use `(version &optional callback)`

Use `version`. If `callback` is specified, use `version` in that
callback and then switch back to the previously used version.

### nvm-use-for `(&optional path callback)`

Read version from `.nvmrc` in `path` (or `default-directory`) and use
that. Second `callback` argument is same as for `nvm-use`.

### nvm-use-for-buffer `()`

Call `nvm-use-for` on the file visited by the current buffer. Suitable
for use in a mode hook to automatically activate the correct node
version for a file.

## Contribution

Contribution is much welcome!

Install [cask](https://github.com/cask/cask) if you haven't
already, then:

    $ cd /path/to/nvm.el
    $ cask

Run all tests with:

    $ make
