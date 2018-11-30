[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/copyleft/gpl.html)

# emacs-ibuffer-project

Emacs package that provides ibuffer filtering and sorting functions to group buffers by project or by default directory.

## Usage

To group buffers by project set `ibuffer-filter-groups` to result of `ibuffer-project-generate-filter-groups` function:
```elisp
(add-hook 'ibuffer-hook
  (lambda ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))))
```

### Project relative filename

This package also provides column with filename relative to project. If there are no file in buffer then column will display buffer name.

Add project-file-relative to `ibuffer-formats`:
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

## Installation

### With `package.el`

Download `ibuffer-project.el` and run:

<kbd>M-x</kbd> `package-install-file` <kbd>RET</kbd> `<path-to-ibuffer-project-el>` <kbd>RET</kbd>
