[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/company-emojify-badge.svg)](https://melpa.org/#/company-emojify)
[![MELPA Stable](https://stable.melpa.org/packages/company-emojify-badge.svg)](https://stable.melpa.org/#/company-emojify)

# company-emojify
> Company completion for Emojify

[![CI](https://github.com/jcs-elpa/company-emojify/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/company-emojify/actions/workflows/test.yml)

## Features

* Uses [emojify](https://github.com/iqbalansari/emacs-emojify)
* Support both `unicode` and `image` displays

## 💾 Quickstart

```el
(require 'company-emojify)
(add-to-list 'company-backends 'company-emojify)
```

## 🔨 Configuration

#### `company-emojify-emoji-styles`

Display these emoji styles as candidates, default to `'(ascii unicode github)`.

If you want to limit to a certain style, you can do the following

```el
(setq company-emojify-emoji-styles '(github))  ; Show only the `github` style
```

See [emojify-emoji-styles]() for more information.

#### `company-emojify-annotation`

Option to display emoji in annotation. It can either be one of the following values,

* `nil`
* `unicode`
* `image`  (default)

Notice, it will display `unicode` if you are in non-graphical environment (terminal).

#### `company-emojify-document`

Display information about the emoji in document buffer, default to `t`.

## ❓ FAQ

#### 💫 How is this different from [company-emoji](https://github.com/dunn/company-emoji)?

`company-emoji` is more lightweight and does not require [emojify](https://github.com/iqbalansari/emacs-emojify).
However, it does not support display with emoji images. `company-emoji` would work
on its own since it declares its emoji list in [company-emoji-list.el](https://github.com/dunn/company-emoji/blob/trunk/company-emoji-list.el).
This package reuses the code from `emojify` hence this would be a better
choice if you already have `emojify` installed.

#### 💫 How to add more emoji?

Unlike `company-emoji` has specify it's own emoji list. Since we rely on
package `emojify`, please consider contribute to the upstream.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
