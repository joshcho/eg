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

;; TODOS:
;; 1. Add a way for deferred operations to halt if a new one is called. Probably using gensym and id's?
;; 2. Better name than 'eg-deferred'
;; 3. Show which function is running at the moment, with something like function-name => ?
;; 4. Animation in overlay using deferred?
;; 5. Propertize strings based on mode?

;; BUGS:
;; 1. eg-format is broken, needs refactoring and printing strings properly (see examples with 'format')

;;; Code:
(require 'dash)
(require 'deferred)

(defvar eg-run-examples-default 3
  "If 'eg-run-examples' is called without numerical argument, set argument to this value. See 'eg-run-examples' for options.")

(defun eg-run-examples (&optional ARG)
  "Run examples for operator under cursor. See 'eg--operator' for what operator means.

When ARG is 1, show examples inline without evaluation.
When ARG is 2, evaluate examples inline and show results when all your examples are evaluated.
When ARG is 3, evaluate examples inline and display results when they are available.
When ARG is 0, ask for the function before evaluating. Evaluates according to 'eg-run-examples-default'."
  (interactive "P")
  (unless ARG (setq ARG eg-run-examples-default))
  (let* ((op (if (= ARG 0)
                 (progn
                   (setq ARG eg-run-examples-default)
                   (eg--ask-for-function "Function to Run: "))
               (aif (eg--operator)
                   it
                 (eg--ask-for-function "Function to Run: "))))
         (examples (eg--get-examples op)))
    (save-excursion
      (if (member major-mode '(lisp-mode emacs-lisp-mode))
          (lispy--back-to-paren)
        (back-to-indentation))
      (unless (and (prog1 (lispy--cleanup-overlay)
                     (when (window-minibuffer-p)
                       (window-resize (selected-window) -1)))
                   (= lispy-hint-pos (point)))
        (setq lispy-hint-pos (point))
        (cond ((null op)
               (eg--show-no-operator))
              ((null examples)
               (eg--show-no-examples op))
              ((= ARG 1) (eg--show-inline examples))
              ((= ARG 2) (eg--eval-inline examples))
              ((= ARG 3) (eval `(eg--deferred-eval-inline ,examples))))))))

(defun eg--show-no-operator ()
  (eg--show-strings-inline (list (format "No expression under cursor."))))

(defun eg--show-no-examples (op)
  (eg--show-strings-inline (list (format "No examples associated with %s."
                                         op))))

(defun eg--show-inline (examples)
  "Show EXAMPLES inline."
  (eg--show-strings-inline
   (cl-loop for e in examples
            collect (format "%s" e))))

(defun eg-eval (example)
  (cl-case major-mode
    ('emacs-lisp-mode (eval example))
    ('lisp-mode (lispy--eval-lisp (prin1-to-string example)))
    ('python-mode (lispy--eval-python example))
    (t (error "%s isn't supported" major-mode))))

(defvar eg-eval-result-max-length 40)
(defun eg--eval-inline (examples)
  "Evaluate EXAMPLES in order and display results inline at the end. See 'eg--deferred-eval-inline' for displaying evaluated results as soon as they are available."
  (eg--show-strings-inline
   (cl-loop for e in examples
            collect
            (eg-format e
                       (condition-case nil
                           (eg-eval e)
                         (error "Eval error"))))))

(defun eg-format (example result)
  (when (and result (< eg-eval-result-max-length (length (prin1-to-string result))))
    (setq result (format "%s..." (substring (prin1-to-string result) 0 eg-eval-result-max-length))))
  (if (and (eql major-mode 'python-mode) (equal result ""))
      (format "%s" example)
    (format "%s => %s" example result)))

(defmacro eg--deferred-eval-inline (examples)
  "Evaluate EXAMPLES in order and display results inline as soon as they are available."
  `(progn
     (eg--show-strings-inline (list (eg-format ',(first examples) "?")) ,(length examples))
     (deferred:$
      (deferred:next
       (lambda ()
         (let* ((e ',(first examples))
                (res (condition-case nil
                         (eg-eval e)
                       (error "Eval error"))))
           (eg--show-strings-inline (list (eg-format e res)) ,(length examples))
           (list res))))
      ,@(cl-loop
         with n = (length examples)
         for i from 1 below n
         collect
         `(deferred:nextc it
            (lambda (res-acc)
              (eg--show-strings-inline (mapcar* (lambda (e res) (eg-format
                                                            e res))
                                                ',(-take (+ i 1) examples)
                                                (-snoc res-acc "?"))
                                       ,n)
              res-acc))
         collect
         `(deferred:nextc it
            (lambda (res-acc)
              (setq res-acc (-snoc res-acc (condition-case nil
                                               (eg-eval ',(nth i examples))
                                             (error "Eval error"))))
              (eg--show-strings-inline (mapcar* (lambda (e res) (eg-format
                                                            e res))
                                                ',(-take (+ i 1) examples)
                                                res-acc)
                                       ,n)
              res-acc))))))

(defvar eg-align-test-results t)
(defun eg--align-strings (strings substring)
  "Align STRINGS that have SUBSTRING by SUBSTRING."
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
  "Show STRINGS inline with vertical padding of DISPLAY-HEIGHT."
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
  :keymaps '(lisp-mode-map emacs-mode-map lispy-mode-map python-mode-map)
  "C-+" 'eg-run-examples
  "C-4" (lambda ()
          (interactive)
          (eg-run-examples 0)))

(provide 'eg-deferred)

;;; eg-deferred.el ends here
