;;; eg-async.el --- Run eg examples asynchronously -*- lexical-binding: t -*-
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
;; 1. Add a way for previous async operations to halt if a new one is called.
;; 2. Refactor async-map-eval
;; 3. Needs polish
;; 4. Can't run any functions I defined. I think this just can't work due to Emacs limitations.

;;; Code:

(defmacro async-let* (bindings &rest body)
  "Same as async-let, but moves to the actual current buffer before executing BODY after BINDINGS."
  (let ((buf (gensym)))
    `(progn
       (setq ,buf (current-buffer))
       (async-let ,bindings
         (switch-to-buffer ,buf)
         ,@body))))

;; (setq eg-async-id (gensym))
(defmacro async-map-eval (expressions to-string handler &optional acc)
  "Evaluate EXPRESSIONS in order and accumulate their results, calling HANDLER at each accumulation. SYM is used as accumulated value."
  (if (equal expressions '())
      '(message "finished running examples")
    (let ((e (gensym))
          (passing-var (gensym)))
      `(async-let*
           ((,e (,(cadr to-string) ',(car expressions) ,(car expressions))))
         (setq ,passing-var (append ,acc (list ,e)))
         (,(cadr handler) ,passing-var ',(cdr expressions))
         (async-map-eval ,(cdr expressions) ,to-string ,handler ,passing-var)))))

(defun eg-run-examples-async ()
  (interactive)
  (let ((examples (eg--get-examples (eg--operator))))
    (if (null examples)
        (format "No examples associated with %s."
                (funcall (if eg-print-functions-uppercase
                             (-compose #'upcase #'prin1-to-string)
                           #'identity)
                         (eg--operator)))
      (eval
       `(async-map-eval
         ,examples
         #'(lambda (expr res)
             (format "%s => %s" expr res))
         #'lispy--show-strings-inline)))))

(defun lispy--show-strings-inline (strings expressions-left)
  (save-excursion
    (goto-char lispy-hint-pos)
    (lispy--show (propertize (string-join (append strings (repeat-list "..." (length expressions-left))) "\n") 'face 'lispy-face-hint))))

(defun repeat-list (x n)
  (if (zerop n)
      '()
    (cons x (repeat-list x (- n 1)))))

(defun lispy--describe-inline ()
  "Toggle the overlay hint."
  (condition-case nil
      (let ((new-hint-pos (lispy--hint-pos))
            doc)
        (if (and (eq lispy-hint-pos new-hint-pos)
                 (overlayp lispy-overlay))
            (lispy--cleanup-overlay)
          (save-excursion
            (when (= 0 (count-lines (window-start) (point)))
              (recenter 1))
            (setq lispy-hint-pos new-hint-pos)
            (if (eq major-mode 'scheme-mode)
                (geiser-doc-symbol-at-point)
              (when (setq doc (lispy--docstring (lispy--current-function)))
                (goto-char lispy-hint-pos)
                (lispy--show (propertize doc 'face 'lispy-face-hint)))))))
    (error
     (lispy--cleanup-overlay))))

(general-def
  :keymaps '(lisp-mode-map emacs-mode-map lispy-mode-map)
  "C-4" 'eg-run-examples-async)

(provide 'eg-async)

;;; eg-async.el ends here
