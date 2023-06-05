[![Stand With Ukraine](https://raw.githubusercontent.com/vshymanskyy/StandWithUkraine/main/banner2-direct.svg)](https://stand-with-ukraine.pp.ua)

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/copyleft/gpl.html)
[![MELPA](http://melpa.org/packages/ibuffer-project-badge.svg)](http://melpa.org/#/ibuffer-project)
[![MELPA Stable](https://stable.melpa.org/packages/ibuffer-project-badge.svg)](https://stable.melpa.org/#/ibuffer-project)

# emacs-ibuffer-project

Emacs package that provides ibuffer filtering and sorting functions to group buffers by custom functions or regexps. By default buffers are grouped by project or by default directory.

## Usage

To group buffers set `ibuffer-filter-groups` to result of `ibuffer-project-generate-filter-groups` function:
```elisp
(add-hook
 'ibuffer-hook
 (lambda ()
   (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
   (unless (eq ibuffer-sorting-mode 'project-file-relative)
     (ibuffer-do-sort-by-project-file-relative))))
```

### Custom groups

Creating custom groups is possible by customizing `ibuffer-project-root-functions`. For example, add function `file-remote-p` like this:
```elisp
(add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote"))
```
In this case all remote buffers will be grouped by a string identifying the remote connection.

### Project relative filename

This package also provides column with filename relative to project. If there are no file in buffer then column will display buffer name.

Add `project-file-relative` to `ibuffer-formats`:
```elisp
(custom-set-variables
 '(ibuffer-formats
   '((mark modified read-only locked " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " project-file-relative))))
```

It's also possible to sort buffers by that column by calling `ibuffer-do-sort-by-project-file-relative` or:
```elisp
(add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
            (unless (eq ibuffer-sorting-mode 'project-file-relative)
              (ibuffer-do-sort-by-project-file-relative))))
```

### Caching project per directory

To avoid calling `project-current` each time, one can set `ibuffer-project-use-cache`. Project info will be stored in the `ibuffer-project-cache` variable. Command `ibuffer-project-clear-cache` allows to clear project info cache.

## Installation

### With `package.el`

`ibuffer-project` available on [MELPA](http://melpa.org):

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `ibuffer-project` <kbd>RET</kbd>.

Alternatively, you can download `ibuffer-project.el` and run:

<kbd>M-x</kbd> `package-install-file` <kbd>RET</kbd> `<path-to-ibuffer-project-el>` <kbd>RET</kbd>
