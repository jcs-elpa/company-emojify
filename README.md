[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CI](https://github.com/jcs-elpa/company-emojify/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/company-emojify/actions/workflows/test.yml)

# company-emojify
> Company completion for Emojify

## Features

* Uses [emojify](https://github.com/iqbalansari/emacs-emojify)
* Support both `unicode` and `image` displays

## :floppy_disk: Quickstart

```el
(require 'company-emojify)
(add-to-list 'company-backends 'company-emoji)
```

## :hammer: Configuration

#### `company-emojify-annotation`

Option to display emoji in annotation. It can either be one of the following values,

* `nil`
* `unicode`
* `image`  (default)

Notice, it will display `unicode` if you are in non-graphical environment (terminal).

#### `company-emojify-document`

Display information about the emoji in document buffer.

## :question: FAQ

#### :dizzy: How is this different from [company-emoji](https://github.com/dunn/company-emoji)?

`company-emoji` is more lightweight and does not require [emojify](https://github.com/iqbalansari/emacs-emojify).
However, it does not support display with emoji images. `company-emoji` would work
on its own since it declares its emoji list in [company-emoji-list.el](https://github.com/dunn/company-emoji/blob/trunk/company-emoji-list.el).
This package reuses the code from `emojify` hence this would be a better
choice if you already have `emojify` installed.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
