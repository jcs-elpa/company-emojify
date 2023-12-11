[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/company-emojify-badge.svg)](https://melpa.org/#/company-emojify)
[![MELPA Stable](https://stable.melpa.org/packages/company-emojify-badge.svg)](https://stable.melpa.org/#/company-emojify)

# company-emojify
> Company completion for Emojify

[![CI](https://github.com/jcs-elpa/company-emojify/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/company-emojify/actions/workflows/test.yml)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [company-emojify](#company-emojify)
  - [🏆 Features](#🏆-features)
  - [💾 Quickstart](#💾-quickstart)
  - [🔨 Configuration](#🔨-configuration)
    - [-](#-)
    - [🔍 `company-emojify-emoji-styles`](#🔍-company-emojify-emoji-styles)
    - [🔍 `company-emojify-annotation`](#🔍-company-emojify-annotation)
    - [🔍 `company-emojify-document`](#🔍-company-emojify-document)
  - [❓ FAQ](#❓-faq)
    - [-](#--1)
    - [💫 How to add more emoji?](#💫-how-to-add-more-emoji)
  - [🛠️ Contribute](#🛠️-contribute)
    - [🔬 Development](#🔬-development)
  - [⚜️ License](#⚜️-license)

<!-- markdown-toc end -->

## 🏆 Features

* Uses [emojify](https://github.com/iqbalansari/emacs-emojify)
* Support both `unicode` and `image` displays

## 💾 Quickstart

```el
(require 'company-emojify)
(add-to-list 'company-backends 'company-emojify)
```

## 🔨 Configuration

#### 🔍 `company-emojify-insert-unicode`

Replace the `:shortcode:` with the real Unicode character upon completion.

#### 🔍 `company-emojify-emoji-styles`

Display these emoji styles as candidates, default to `'(ascii unicode github)`.

If you want to limit to a certain style, you can do the following

```el
(setq company-emojify-emoji-styles '(github))  ; Show only the `github` style
```

See [emojify-emoji-styles]() for more information.

#### 🔍 `company-emojify-annotation`

Option to display emoji in annotation. It can either be one of the following values,

* `nil`
* `unicode`
* `image`  (default)

Notice, it will display `unicode` if you are in non-graphical environment (terminal).

#### 🔍 `company-emojify-document`

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

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test the package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
