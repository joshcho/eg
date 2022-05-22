;;; eg-deferred.el --- Run eg examples deferred -*- lexical-binding: t -*-
;;
;; Copyright © 2022-2022 Jungmin "Josh" Cho
;;
;; Author: Josh Cho <joshchonpc@gmail.com>
;; URL: https://github.com/joshcho/eg
;; Version: alpha
;; Keywords: convenience
;; Package-Requires: (lispy cl-format) FIXME: remove dependencies

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Add, remove, or run examples for a function. Displays examples
;; right there, inline, to lessen context-switching.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; TODO:
;; 1. Add a way for deferred operations to halt if a new one is called. Probably using gensym and id's?
;; 2. Better name than 'eg-deferred'

;;; Code:
(require 'dash)
(require 'deferred)

(defvar eg-examples-running nil)
(defun eg-run-examples-incrementally ()
  "Run examples incrementally, updating inline display as soon as results are available. Running this while examples are running will stop remaining tasks as soon as possible."
  (interactive)
  (let ((examples (eg--get-examples (eg--operator))))
    (if (null examples)
        (lispy--show-inline (format "No examples associated with %s."
                                    (funcall (if eg-print-functions-uppercase
                                                 (-compose #'upcase #'prin1-to-string)
                                               #'identity)
                                             (eg--operator))))
      (save-excursion
        (lispy--back-to-paren)
        (unless (and (prog1 (lispy--cleanup-overlay)
                       (when (window-minibuffer-p)
                         (window-resize (selected-window) -1)))
                     (= lispy-hint-pos (point)))
          (cond ((memq major-mode lispy-elisp-modes)
                 (setq lispy-hint-pos (point))
                 (eval `(eg--deferred-eval-inline ,examples)))
                (t (error "%s isn't supported currently" major-mode))))))))

(defmacro eg--deferred-eval-inline (examples)
  "Generates deferred chain where EXAMPLES are evaluated in order, and displayed inline using 'lispy--show-strings-inline' as soon as results are available."
  `(progn
     (lispy--show-strings-inline '("...") ,(length examples))
     (deferred:$
      (deferred:next
       (lambda ()
         (let* ((e ',(first examples))
                (res (prin1-to-string
                      (eval e))))
           (lispy--show-strings-inline (list (format "%s => %s" e res)) ,(length examples))
           (list res))))
      ,@(cl-loop
         with n = (length examples)
         for i from 1 below n
         collect
         `(deferred:nextc it
            (lambda (res-acc)
              (setq res-acc (-snoc res-acc (eval ',(nth i examples))))
              (lispy--show-strings-inline (mapcar* (lambda (e res) (format "%s => %s"
                                                                      e res))
                                                   ',(-take (+ i 1) examples)
                                                   res-acc)
                                          ,n)
              res-acc))))))

(defun lispy--show-strings-inline (strings display-height)
  "Incrementally show STRINGS inline in display of DISPLAY-HEIGHT"
  (save-excursion
    (goto-char lispy-hint-pos)
    (lispy--show (propertize (string-join (append strings (-repeat (- display-height (length strings)) "...")) "\n") 'face 'lispy-face-hint))))

(general-def
  :keymaps '(lisp-mode-map emacs-mode-map lispy-mode-map)
  "C-+" 'eg-run-examples-incrementally)

(provide 'eg-deferred)

;;; eg-deferred.el ends here
