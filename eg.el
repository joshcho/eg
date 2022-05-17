;;; eg.el --- Access examples (e.g. eg) on the fly -*- lexical-binding: t -*-
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

;;; Code:

;; Bugs
;; 1. If a function is not defined, any eg-whatever in (function @args) doesn't work.
;; 2. Gracefully handle if an example is broken
;; 3. Remove examples is broken
;; 4. Functions that use lispy--show-inline are all broken.

;; TODO
;; 1. Make multiple examples at once possible
;; 2. Evaluation for other languages
;; 3. Pop minibuffer to allow editing of examples, i.e. eg-edit-examples
;; 4. Add expected value?
;; 5. Provide an option to evaluate current expression as well when calling eg-run-examples
;; 6. Make data structure of eg-examples language-dependent
;; 7. Align test results
;; 8. Show recommended function first (instead of last)
;; 9. Support multiline tests
;; 10. Multiline support for long sexps
;; 11. Live-updating minibuffer

(require 'lispy)
(require 'cl-format)
(require 'thingatpt)

(defgroup eg nil
  "eg configuration."
  :prefix "eg-"
  :group 'convenience)

(defun eg--file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; used in conjunction with lispy--cleanup-overlay
(defmacro lispy--show-inline (expr)
  "Display the result of EXPR inline. EXPR must evaluate to string. This is implemented as a macro as EXPR must be evaluated after the program traverses to the appropriate location. FIXME: Really weird interaction."
  `(save-excursion
     (lispy--back-to-paren)
     (unless (and (prog1 (lispy--cleanup-overlay)
                    (when (window-minibuffer-p)
                      (window-resize (selected-window) -1)))
                  (= lispy-hint-pos (point)))
       (cond ((memq major-mode lispy-elisp-modes)
              (let ((sym (intern-soft (lispy--current-function))))
                (cond ((fboundp sym)
                       (setq lispy-hint-pos (point))
                       (lispy--show (eval ,expr))
                       (other-window 1)))))
             ((eq major-mode 'lisp-mode)
              (require 'le-lisp)
              (setq lispy-hint-pos (point))
              (lispy--show (eval ,expr))
              (other-window 1))
             (t (error "%s isn't supported currently" major-mode))))))

(defvar eg-file (expand-file-name "~/eg/eg-examples.el"))

(defvar eg-examples nil
  "Examples associated with functions. Do not edit eg-examples manually; use eg--update-examples and eg--get-examples.")

(defvar eg-examples-doc "eg-examples. changes made by hand may be overwritten. if you made changes, make sure to call eg-load-examples to sync eg-examples with your file."
  "That which gets added to the beginning of eg-examples.el.")

(defun eg--prettify-eg-examples ()
  (mapcar (lambda (s)
            (cons (car s)
                  (mapcar (lambda (g) (cons (car s) g)) (cdr s))))
          eg-examples))

(defun eg-load-examples ()
  "Load eg-examples from eg-file."
  (interactive)
  (setq eg-examples (read (eg--file-to-string eg-file)))
  (eg--prettify-eg-examples))

(defun eg-save-examples ()
  "Save current state of eg-examples (all your examples) to your eg-file."
  (interactive)
  (with-temp-buffer
    (find-file eg-file)
    (erase-buffer)
    (insert ";; " eg-examples-doc "\n")
    (insert (prin1-to-string eg-examples))
    (lispy-multiline)  ; FIXME: get rid of this dependency if possible
    (save-buffer)
    (kill-buffer)))

(defvar eg-save-on-every-update nil
  "If t, then eg saves every time add or remove occurs. Otherwise, you have to save current state of examples manually through eg-save-examples.")
(defvar eg-ask-save-on-exit t
  "If t, ask to save on exiting emacs.")

(add-hook 'kill-buffer-hook (lambda () (when (and eg-ask-save-on-exit
                                             eg-examples ; check that it is loaded
                                             (not (equal eg-examples (read (eg--file-to-string eg-file))))
                                             (yes-or-no-p (format "eg: Save your examples to %s?" eg-file)))
                                    (eg-save-examples))))

(defvar eg-load-on-startup t)
(when eg-load-on-startup
  (eg-load-examples))

(defun eg-visit-examples-file ()
  (interactive)
  (find-file eg-file))

(defun eg--current-list ()
  "Get current list around point. FIXME: Not a perfect solution for handling different positions cursor could be in."
  (save-excursion
    (unless (thing-at-point 'list)
      ;; not a perfect solution
      (backward-char))
    (if (thing-at-point 'list)
        (let* ((current-list (read (thing-at-point 'list)))
               (current-list-string-p (symbolp current-list)))
          (if current-list-string-p
              (progn
                ;; not a perfect solution
                (back-to-indentation)
                (read (thing-at-point 'list)))
            current-list))
      nil)))

(defun eg--def-p (expr)
  "Check if EXPR is def expression (e.g. defun, defmacro, defgeneric)."
  (member (first expr) '(defun defmacro defgeneric)))

(defun eg--operator (&optional expr)
  "Returns the operator in EXPR."
  (unless expr (setq expr (eg--current-list)))
  (when expr
    (when (equal (first expr) 'quote)
      (setq expr (second expr)))
    (if (eg--def-p expr)
        (second expr)
      (first expr))))

(defun eg--args (&optional expr)
  "Returns the args in EXPR."
  (unless expr (setq expr (eg--current-list)))
  (when expr
    (when (equal (first expr) 'quote)
      (setq expr (second expr)))
    (if (eg--def-p expr)
        (third expr)
      (rest expr))))

(defun eg--example-template (&optional expr)
  (unless expr (setq expr (eg--current-list)))
  (cons (eg--operator expr) (eg--args expr)))

(defun eg--get-examples (fn)
  "Get examples associated with FN in eg-examples."
  (mapcar (lambda (s) (cons fn s)) (cdr (assoc fn eg-examples))))

(defun eg--update-examples (fn examples)
  "Set examples associated with FN to EXAMPLES."
  (if examples
      (if (cdr (assoc fn eg-examples))
          (setf (cdr (assoc fn eg-examples))
                (mapcar #'cdr examples))
        (setq eg-examples (cons (cons fn (mapcar #'cdr examples))
                                eg-examples)))
    (setq eg-examples
          (cl-loop for pair in eg-examples
                   unless (equal (car pair) fn)
                   collect pair)))
  examples)

(defun eg--get-functions ()
  (mapcar #'car eg-examples))

(defvar eg-print-functions-uppercase nil
  "Whether to print functions in upper case.")

(defun eg--examples-to-string (fn)
  "Get string of examples associated with FN."
  (let ((examples (eg--get-examples fn)))
    (if examples
        (string-join (mapcar #'prin1-to-string examples)
                     "\n")
      (format "No examples associated with %s."
              (if eg-print-functions-uppercase
                  (upcase (prin1-to-string fn))
                fn)))))

(defun eg--run-examples-to-string (fn)
  "Get string of examples associated with FN with run."
  (let ((examples (eg--get-examples fn)))
    (if examples
        (string-join (mapcar (lambda (e)
                               (concat (prin1-to-string e)
                                       " => "
                                       (lispy--eval (prin1-to-string e))))
                             examples)
                     "\n")
      (format "No examples associated with %s."
              (if eg-print-functions-uppercase
                  (upcase (prin1-to-string fn))
                fn)))))

(defun eg-show-examples-prompt ()
  "Prompt for a function. Show examples associated with that function."
  (interactive)
  (let ((str (eg--examples-to-string
              (eg--completing-read-sexp "Function to show examples for: " (eg--get-functions) t))))
    (lispy--show-inline str)))


(defun eg-run-examples-prompt ()
  "Prompt for a function. Run and show examples associated with that function."
  (interactive)
  (unless fn (setq fn (eg--operator)))
  (let ((str (eg--run-examples-to-string
              (eg--completing-read-sexp "Function to show examples for: " (eg--get-functions) t))))
    (lispy--show-inline str)))

(defun eg-show-examples ()
  "Show examples associated with contextual operator."
  (interactive)
  (let ((str (eg--examples-to-string (eg--operator))))
    (lispy--show-inline str)))

(defun eg-run-examples ()
  "Run and show examples associated with contextual operator."
  (interactive)
  (let ((str (eg--run-examples-to-string (eg--operator))))
    (lispy--show-inline str)))

(defun eg--parse-args (operands)
  "Parse arguments for OPERANDS."
  (remove-if #'listp
             (cl-loop for o in operands
                      unless (member o '(&optional))
                      collect o)))

;; (defvar eg-add-complete-p t "FIXME: Support later. Complete function when adding example.")
;; (defvar eg-add-complete-args-p nil   "FIXME: Support later. Complete arguments when adding example. For this to be true, eg-add-complete-p must be true as well.")

(defun eg--completing-read-sexp (prompt collection)
  "Do a completing-read with PROMPT on COLLECTION."
  (read (completing-read prompt
                         (mapcar #'prin1-to-string
                                 collection))))

(defun eg--read-from-minibuffer-sexp (prompt initial-value)
  "Do a read-from-minibuffer with PROMPT on COLLECTION."
  (read (read-from-minibuffer prompt
                              initial-value)))

(defun eg--ask-for-function (prompt)
  (eg--completing-read-sexp prompt (adjoin (eg--operator) (eg--get-functions))))

(defun eg--perform-add-example (example fn)
  "Add EXAMPLE to FN."
  (unless (member example (eg--get-examples fn))
    (eg--update-examples fn (append (eg--get-examples fn)
                                    (list example)))))

(defun eg--query-example-for-add (fn)
  (eg--read-from-minibuffer-sexp "Example to add: "
                                 (when (equal (first (eg--current-list)) fn)
                                   (prin1-to-string (eg--current-list)))))

(defun eg-add-example ()
  "Interactive add."
  (interactive)
  (eg-modify-example "Associated function: "
                     #'eg--query-example-for-add
                     #'eg--perform-add-example
                     "Added %s from %s"
                     "%s already an example in %s"))

(defun eg--perform-remove-example (example fn)
  "Remove EXAMPLE from FN. Returns t if removed, nil otherwise."
  (let ((examples (eg--get-examples fn)))
    (eg--update-examples fn (remove example (eg--get-examples fn)))
    (not (equal examples (eg--get-examples fn)))))

(defun eg--query-example-for-remove (fn)
  (eg--completing-read-sexp "Example to remove: "
                            (if (equal (eg--operator) fn)
                                (adjoin (eg--operator) (eg--get-examples fn))
                              (eg--get-examples fn))))

(defun eg-remove-example ()
  "Interactive removal."
  (interactive)
  (eg-modify-example "Associated function: "
                     #'eg--query-example-for-remove
                     #'eg--perform-remove-example
                     "Removed %s from %s"
                     "Could not find example %s in %s"))

(defun eg-get-examples ()
  (interactive)
  (message "%s" (eg--get-examples (eg--ask-for-function "Which function to fetch examples from: "))))

(defun eg-modify-example (fn-prompt get-example perform-function success-prompt fail-prompt)
  "PERFORM-FUNCTION takes example and fn to modify eg-examples. This function is used as template for eg-add-example and eg-remove-example. GET-EXAMPLE takes in fn."
  (let* ((fn (eg--ask-for-function fn-prompt))
         (example (funcall get-example fn)))
    (if (funcall perform-function example fn)
        (message success-prompt example fn)
      (message fail-prompt example fn)))
  (when eg-save-on-every-update
    (eg-save-examples)))

(defun eg-print-examples (&optional fn)
  "Print examples associated with FN. If FN is nil, set FN to operator of current list."
  (interactive)
  (unless fn (setq fn (eg--operator)))
  (message (eg--examples-to-string fn)))

(define-prefix-command 'eg-command-map)
(global-set-key (kbd "C-c C-e") 'eg-command-map)

(general-def
  :keymaps 'lispy-mode-map
  "C-+" 'eg-run-examples)

(general-def
  :prefix "C-c C-e"
  :keymaps 'lispy-mode-map
  "a" 'eg-add-example
  "C-a" 'eg-add-example
  "r" 'eg-run-examples
  "C-r" 'eg-run-examples
  "S-r" 'eg-remove-examples
  "C-S-r" 'eg-remove-examples
  )

(general-def
  :keymaps '(emacs-lisp-mode-map eshell-mode-map)
  "C-c C-z" 'eshell-toggle
  )
