;;; ibuffer-project.el --- Group ibuffer's list by project -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Andrii Kolomoiets

;; Author: Andrii Kolomoiets <andreyk.mad@gmail.com>
;; Keywords: tools
;; URL: https://github.com/muffinmad/emacs-ibuffer-project
;; Package-Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This pacakage provides ibuffer filtering and sorting functions to group buffers
;; by project or by `default-directory'
;;
;; To group buffers by project set `ibuffer-filter-groups' to result of
;; `ibuffer-project-generate-filter-groups' function:
;;
;;    (add-hook 'ibuffer-hook
;;              (lambda ()
;;                (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))))
;;
;; This package also provides column with filename relative to project. If there are no
;; file in buffer then column will display `buffer-name' with `font-lock-comment-face' face.
;; Add project-file-relative to `ibuffer-formats':
;;
;;    (custom-set-variables
;;     '(ibuffer-formats
;;       '((mark modified read-only locked " "
;;               (name 18 18 :left :elide)
;;               " "
;;               (size 9 -1 :right)
;;               " "
;;               (mode 16 16 :left :elide)
;;               " " project-file-relative))))
;;
;; It's also possible to sort buffers by that column by calling `ibuffer-do-sort-by-project-file-relative'
;; or:
;;
;;    (add-hook 'ibuffer-hook
;;              (lambda ()
;;                (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
;;                (unless (eq ibuffer-sorting-mode 'project-file-relative)
;;                  (ibuffer-do-sort-by-project-file-relative))))

;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)

(defun ibuffer-project-root (buf)
  "Return a cons cell (project-root . root-type) for BUF."
  (with-current-buffer buf
    (unless (string-match-p "^ " (buffer-name))
      (let ((dir (cdr (project-current))))
        (cond
         (dir (cons dir 'project))
         (default-directory (cons (abbreviate-file-name default-directory) 'directory)))))))

(defun ibuffer-project-group-name (root type)
  "Return group name for project ROOT and TYPE."
  (format "%s: %s" (if (eq type 'project) "Project" "Directory") root))

(define-ibuffer-filter project-root
    "Toggle current view to buffers with project root dir QUALIFIER."
  (:description "project root dir"
                :reader (read-regexp "Filter by project root dir (regexp): "))
  (ibuffer-awhen (ibuffer-project-root buf)
    (if (stringp qualifier)
        (string-match-p qualifier (car it))
      (equal qualifier it))))

;;;###autoload (autoload 'ibuffer-do-sort-by-project-file-relative "ibuffer-project")
(define-ibuffer-sorter project-file-relative
  "Sort by filename relative to project"
  (:description "project filename relative")
  (let* ((bufa (car a))
         (bufb (car b))
         (filea (with-current-buffer bufa buffer-file-name))
         (fileb (with-current-buffer bufb buffer-file-name)))
    (cond
     ((and filea fileb) (string-lessp filea fileb))
     ((or filea fileb) (null fileb))
     (t (string-lessp (buffer-name bufa) (buffer-name bufb))))))

(defvar ibuffer-project-file-relative-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-1)] 'ibuffer-do-sort-by-project-file-relative)
    map)
  "Mouse keymap for filename relative to project column.")

;;;###autoload (autoload 'ibuffer-make-column-project-file-relative "ibuffer-project")
(define-ibuffer-column project-file-relative
  (:name "Filename"
         :header-mouse-map ibuffer-project-file-relative-header-map)
  (if buffer-file-name
      (let ((root (car (ibuffer-project-root buffer))))
        (if root
            (file-relative-name buffer-file-name root)
          (abbreviate-file-name buffer-file-name)))
    (propertize (buffer-name) 'font-lock-face 'font-lock-comment-face)))

;;;###autoload
(defun ibuffer-project-generate-filter-groups ()
  "Create ibuffer filters based on project root of buffers."
  (let ((roots (ibuffer-remove-duplicates
                (delq nil (mapcar 'ibuffer-project-root (buffer-list))))))
    (mapcar (lambda (root)
              (cons (ibuffer-project-group-name (car root) (cdr root))
                    `((project-root . ,root))))
            roots)))

(provide 'ibuffer-project)
;;; ibuffer-project.el ends here
