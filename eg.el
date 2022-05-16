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

;; TODO
;; 1. Make multiple examples at once possible
;; 2. Evaluation for other languages
;; 3. Pop minibuffer to allow editing of examples, i.e. eg-edit-examples
;; 4. Add expected value?
;; 5. Provide an option to evaluate current expression as well when calling eg-run-examples
;; 6. Make data structure of eg-examples language-dependent
;; 7. Align test results
;; 8. Show recommended function first (instead of last)

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
  "Display the result of EXPR inline. EXPR must evaluate to string. This is implemented as a macro as EXPR must be evaluated after the program traverses to the appropriate location."
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
(add-hook 'kill-emacs-hook (lambda () (when (and eg-ask-save-on-exit
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
  (lispy--show-inline (eg--examples-to-string
                       (eg--completing-read-sexp "Function to show examples for: " (eg--get-functions) t))))

(defun eg-run-examples-prompt ()
  "Prompt for a function. Run and show examples associated with that function."
  (interactive)
  (unless fn (setq fn (eg--operator)))
  (lispy--show-inline (eg--run-examples-to-string
                       (eg--completing-read-sexp "Function to show examples for: " (eg--get-functions) t))))

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

(defun eg--completing-read-sexp (prompt collection &optional use-current-operator)
  "Do a completing-read with PROMPT on COLLECTION. USE-CURRENT-OPERATOR determines whether result of eg--operator is provided as part of collection."
  (setq fn (read (completing-read prompt
                                  (mapcar #'prin1-to-string
                                          (if use-current-operator
                                              (let ((op (eg--operator)))
                                                (if (member op collection)
                                                    collection
                                                  (cons op collection)))
                                            collection))))))

(defun eg--perform-add-example (example fn)
  "Add EXAMPLE to FN."
  (unless (member example (eg--get-examples fn))
    (eg--update-examples fn (append (eg--get-examples fn)
                                    (list example)))))

(defvar eg-display-existing-examples nil
  "Whether to display existing examples when adding examples. Uses completing-read instead.") ; ergonomics could be improved here
(defun eg-add-example (&optional example fn)
  (interactive)
  (unless fn (setq fn (eg--completing-read-sexp "Associated function for adding example: " (eg--get-functions) t)))
  (let ((initial-value-string (if (equal (first (eg--current-list)) fn)
                                  (prin1-to-string (eg--current-list))))
        (prompt-string (cl-format nil "Example for ~a: " fn)))
    ;; FIXME: Make it possible to add multiple examples at once
    (unless example (setq example
                          (if eg-display-existing-examples
                              (read (eg--completing-read-sexp prompt-string (eg--get-examples fn)))
                            (read (read-from-minibuffer prompt-string initial-value-string)))))
    (if (eg--perform-add-example example fn)
        (message "Added %s to %s" example fn)
      (message "%s already an example in %s" example fn)))
  (when eg-save-on-every-update
    (eg-save-examples)))

(defun eg--perform-remove-example (fn example)
  "Remove EXAMPLE from FN. Returns t if removed, nil otherwise."
  (let ((examples (eg--get-examples fn)))
    (eg--update-examples fn (remove example (eg--get-examples fn)))
    (not (equal examples (eg--get-examples fn)))))

(defun eg-remove-example (&optional fn example)
  "Remove EXAMPLE from FN."
  (interactive)
  (unless fn
    (setq fn (eg--completing-read-sexp "Associated function for example removal: " (eg--get-functions)) t))
  (if (not (eg--get-examples fn))
      (message "No examples associated with %s." fn)
    (progn
      (unless example
        (setq example
              (eg--completing-read-sexp "Example to remove:" (eg--get-examples fn) t)))
      (if (eg--perform-remove-example fn example)
          (message "Removed %s from %s" example fn)
        (message "Could not find example %s in %s" example fn))))
  (when eg-save-on-every-update
    (eg-save-examples)))

(defun eg-print-examples (&optional fn)
  "Print examples associated with FN. If FN is nil, set FN to operator of current list."
  (interactive)
  (unless fn (setq fn (eg--operator)))
  (message (eg--examples-to-string fn)))

(define-key emacs-lisp-mode-map (kbd "C-+") 'eg-run-examples)
(define-key lisp-mode-map (kbd "C-+") 'eg-run-examples)
