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
;; 3. Move this to main file

;;; Code:
(require 'dash)
(require 'deferred)

(defvar eg-run-examples-deferred nil
  "When t, uses 'deferred' to show examples as soon as they are available. Set this to nil if you are only working with small functions.")
(defun eg-run-examples ()
  "Run examples for appropriate function under cursor. Can be customized to run without deferred FIXME: add custom variable and update doc."
  (interactive)
  (let ((examples (eg--get-examples (eg--operator))))
    (if (null examples)
        (lispy--show-inline (format "No examples associated with %s."
                                    (eg--operator)))
      (save-excursion
        (lispy--back-to-paren)
        (unless (and (prog1 (lispy--cleanup-overlay)
                       (when (window-minibuffer-p)
                         (window-resize (selected-window) -1)))
                     (= lispy-hint-pos (point)))
          (cond ((memq major-mode lispy-elisp-modes)
                 (setq lispy-hint-pos (point))
                 (if eg-run-examples-deferred
                     (eval `(eg--deferred-eval-inline ,examples))
                   (eg--eval-inline examples)))
                (t (error "%s isn't supported currently" major-mode))))))))

(defun eg--eval-inline (examples)
  "Evaluate EXAMPLES in order and display results inline at the end. See 'eg--deferred-eval-inline' for displaying evaluated results as soon as they are available."
  (eg--show-strings-inline
   (cl-loop for e in examples
            collect
            (format "%s => %s" e
                    (condition-case nil
                        (eval e)
                      (error "Eval error"))))))

(defmacro eg--deferred-eval-inline (examples)
  "Evaluate EXAMPLES in order and display results inline as soon as they are available."
  `(progn
     (lispy--show-strings-inline '("...") ,(length examples))
     (deferred:$
      (deferred:next
       (lambda ()
         (let* ((e ',(first examples))
                (res (condition-case nil
                         (eval e)
                       (error "Eval error"))))
           (eg--show-strings-inline (list (format "%s => %s" e res)) ,(length examples))
           (list res))))
      ,@(cl-loop
         with n = (length examples)
         for i from 1 below n
         collect
         `(deferred:nextc it
            (lambda (res-acc)
              (setq res-acc (-snoc res-acc (condition-case nil
                                               (eval ',(nth i examples))
                                             (error "Eval error"))))
              (eg--show-strings-inline (mapcar* (lambda (e res) (format "%s => %s"
                                                                   e res))
                                                ',(-take (+ i 1) examples)
                                                res-acc)
                                       ,n)
              res-acc))))))

(defvar eg-align-test-results t)
(defun eg--align-strings (strings substring)
  "Align strings in STRINGS that have SUBSTRING by SUBSTRING."
  (cl-loop for s in strings
           for pos = (cl-search substring s)
           maximize (if pos pos (+ 1 (length s))) into pad-pos
           collect pos into pos-list
           collect (if pos
                       (cons (substring s 0 pos)
                             (substring s pos))
                     (cons s ""))
           into substring-pairs
           finally
           (return (mapcar (lambda (pair)
                             (concat (string-pad (car pair) pad-pos) (cdr pair)))
                           substring-pairs))))

(defun eg--show-strings-inline (strings &optional display-height)
  "Show STRINGS inline in display of DISPLAY-HEIGHT."
  (save-excursion
    (goto-char lispy-hint-pos)
    (lispy--show (propertize (string-join
                              (eg--align-strings
                               (append strings
                                       (when display-height
                                         (-repeat (- display-height (length strings)) "...")))
                               "=>")
                              "\n")
                             'face 'lispy-face-hint))))

(general-def
  :keymaps '(lisp-mode-map emacs-mode-map lispy-mode-map)
  "C-+" 'eg-run-examples)

(provide 'eg-deferred)

;;; eg-deferred.el ends here
