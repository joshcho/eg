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

;; Bugs
;; 1. If a function is not defined, any eg-whatever in (function @args) doesn't work.
;; 2. Gracefully handle if an example is broken
;; 3. Remove examples is broken
;; 4. Functions that use lispy--show-inline are all broken.
;; 5. Weird behavior when calling run-examples (probably) from eg-live-fn buffer

;; TODO
;; 1. Evaluation for other languages (e.g. python)
;; 2. Add expected value?
;; 3. Provide an option to evaluate current expression as well when calling eg-run-examples
;; 4. Align test results
;; 5. Show recommended function first (instead of last)
;; 6. Add interactive add example (like eg-live)
;; 7. Add multiline support for inline displays
;; 8. Allow persistent commented tests
;; 9. Separate examples for different languages (and load them separately, too)
;; 10. Make lang into keyword, not optional
;; 11. Differentiate between my keybindings and general keybindings (use-package just for myself)
;; 12. Fix some porting issues in README (consider markdown)

;; TODO for eg-live
;; 1. Display header instead of comment
;; 2. Think of better distinction between eg-live and eg-live-fn
;; 3. *Live preview of examples as you scroll through, maybe use consult?*
;; 4. Gracefully handle keyboard exits in completing-read as per
;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
;; 5. Generalize personal use with exit-emacs-state
;; 6. Allow adding new fn-examples with eg-live-fn
;; 7. Recover location (forcing save-excursion) when running examples

;;; Code:

(require 'lispy)
(require 'thingatpt)
(require 'ht)

(defgroup eg nil
  "eg configuration."
  :prefix "eg-"
  :group 'convenience)

(defun eg--file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

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
  "Examples associated with functions.")

(defvar eg-examples-doc "eg-examples. changes made by hand may be overwritten. if you made changes, make sure to call eg-load-examples to sync eg-examples with your file."
  "That which gets added to the beginning of eg-examples.el.")

;; (defun eg--prettify-eg-examples ()
;;   (mapcar (lambda (s)
;;             (cons (car s)
;;                   (mapcar (lambda (g) (cons (car s) g)) (cdr s))))
;;           eg-examples))

;; (defun read-multiple (string)
;;   "Read STRING and return a list of its Lisp forms."
;;   (read (format "(%s)" string)))

(defun eg--local->stored (local-examples)
  "Convert LOCAL-EXAMPLES to stored format. See eg--stored->local for explanation."
  (cl-loop for lang being the hash-keys in local-examples
           using (hash-value fn-examples)
           collect (cons lang (ht->alist fn-examples))
           ))

(defun eg--stored->local (stored-examples)
  "Convert STORED-EXAMPLES to local format. eg uses two representations for data. When storing examples in eg-file, eg uses nested associaton list. When loading and using examples in emacs, eg uses nested hash table. This function (and eg--local->stored) converts between the two formats."
  (ht<-alist
   (cl-loop for (lang . fn-examples) in stored-examples
            collect (cons lang (ht<-alist fn-examples)))))

(defun eg--local-examples-equal (examples1 examples2)
  (and
   (= (ht-size examples1)
      (ht-size examples2))
   (cl-loop for lang being the hash-keys in examples1
            always (ht-equal? (ht-get examples1 lang)
                              (ht-get examples2 lang)))))

;; (defun eg--iterate-on-examples (func)
;;   ;; for refactoring
;;   (cl-loop for lang being the hash-keys in eg-examples
;;            do (cl-loop for fn being the hash-keys in (ht-get eg-examples lang)
;;                        do (setf (ht-get* eg-examples lang fn)
;;                                 (mapcar func (ht-get* eg-examples lang fn))))))

(defun eg-load-examples ()
  "Load eg-examples from eg-file."
  (interactive)
  (let ((stored-examples (read (eg--file-to-string eg-file))))
    (setq eg-examples (eg--stored->local stored-examples)))
  ;; (eg--prettify-eg-examples)
  )

(defun eg--funcall-when (pred function argument)
  "Beware that it runs on single ARGUMENT, not arguments."
  (if pred
      (funcall function argument)
    argument))

(defun eg--sort-stored (examples)
  (cl-sort (cl-loop for (lang . fn-examples) in examples
                    collect (cons lang (cl-sort fn-examples #'string< :key #'car)))
           #'string< :key #'car))

(defvar eg-sort-on-save t
  "Whether to sort your examples on save. Can be slow if you have a lot of examples.")
(defun eg--examples-modified ()
  "Check if eg-examples is modified since load."
  (not (eg--local-examples-equal
        eg-examples
        (eg--stored-format->local-format (read (eg--file-to-string eg-file))))))

(defun eg-save-examples ()
  "Save current state of eg-examples to your eg-file."
  (interactive)
  (if (eg--examples-modified)
      (save-window-excursion
        (let ((buffer-already-open (get-file-buffer eg-file)))
          (find-file eg-file)
          (erase-buffer)
          (insert ";; " eg-examples-doc "\n")
          (insert (prin1-to-string
                   (eg--funcall-when eg-sort-on-save
                                     #'eg--sort-stored
                                     (eg--local->stored eg-examples))))
          (lispy-multiline) ; FIXME: get rid of this dependency if possible
          (insert ")")
          (save-buffer)
          (unless buffer-already-open
            (kill-buffer))))
    (message "Examples not modified since load, not saning.")))

(defvar eg-save-on-every-update nil
  "If t, then eg saves every time add or remove occurs. Otherwise, you have to save current state of examples manually through eg-save-examples.")
(defvar eg-ask-save-on-exit t
  "If t, ask to save on exiting emacs.")

(add-hook 'kill-emacs-hook (lambda () (when (and eg-ask-save-on-exit
                                            (eg--examples-modified)
                                            (yes-or-no-p (format "eg: Save your examples to %s?" eg-file)))
                                   (eg-save-examples))))

(defvar eg-load-on-startup t)
(when eg-load-on-startup
  (eg-load-examples))

(defun eg--current-list ()
  "Get current list around point. FIXME: Not a perfect solution for handling different positions cursor could be in."
  (save-excursion
    (unless (thing-at-point 'list)
      ;; not a perfect solution
      (ignore-error (backward-char)))
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
  (member (first expr) '(cl-defun cl-defmacro defun defmacro defgeneric)))

(cl-defun eg--operator (&optional (expr (eg--current-list)))
  "Returns the operator in EXPR."
  (when expr
    (when (equal (first expr) 'quote)
      (setq expr (second expr)))
    (if (eg--def-p expr)
        (second expr)
      (first expr))))

(cl-defun eg--args (&optional (expr (eg--current-list)))
  "Returns the args in EXPR."
  (when expr
    (when (equal (first expr) 'quote)
      (setq expr (second expr)))
    (if (eg--def-p expr)
        (third expr)
      (rest expr))))

(cl-defun eg--example-template (&optional (expr (eg--current-list)))
  (cons (eg--operator expr) (eg--args expr)))

(cl-defun eg--get-examples (fn &optional (lang (eg--current-lang)))
  "Get examples associated with LANG and FN in eg-examples."
  (ht-get* eg-examples lang fn))

(cl-defun eg--update-examples (fn examples &optional (lang (eg--current-lang)))
  "Set examples associated with LANG and FN to EXAMPLES."
  (if (null examples)
      (ht-remove (ht-get eg-examples lang) fn)
    (setf (ht-get* eg-examples lang fn) examples)))

(cl-defun eg--get-functions (&optional (lang (eg--current-lang)))
  (ht-keys (ht-get eg-examples lang)))

(cl-defun eg--get-languages ()
  (ht-keys eg-examples))

(defvar eg-print-functions-uppercase nil
  "Whether to print functions in upper case.")

(cl-defun eg--examples-to-string (fn &optional (print-function (prin1-to-string)) (lang (eg--current-lang)))
  "Get string of examples associated with LANG and FN."
  (let ((examples (eg--get-examples fn lang)))
    (if (null examples)
        (format "No examples associated with %s."
                (if eg-print-functions-uppercase
                    (upcase (prin1-to-string fn))
                  fn))
      (string-join (mapcar print-function examples)
                   "\n"))))

(cl-defun eg--run-examples-to-string (fn &optional (lang (eg--current-lang)))
  "Get string of examples associated with LANG and FN with run."
  (eg--examples-to-string fn #'(lambda (e)
                                 (concat (prin1-to-string e)
                                         " => "
                                         (lispy--eval (prin1-to-string e))))))

(defun eg--current-lang ()
  (if (equal major-mode 'emacs-lisp-mode)
      'emacs-lisp
    'lisp))

(defun eg-show-examples-prompt ()
  "Prompt for a function. Show examples associated with that function."
  (interactive)
  (let ((str (eg--examples-to-string
              (eg--completing-read-sexp "Function to show examples for: " (eg--get-functions)))))
    (lispy--show-inline str)))

(defun eg-run-examples-prompt ()
  "Prompt for a function. Run and show examples associated with that function."
  (interactive)
  (let ((str (eg--run-examples-to-string
              (eg--completing-read-sexp "Function to show examples for: " (eg--get-functions)))))
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

(cl-defun eg--ask-for-function (prompt &optional (lang (eg--current-lang)))
  "Ask for function in PROMPT."
  (eg--completing-read-sexp prompt (append (when-let (op (eg--operator))
                                             (list op))
                                           (eg--get-functions lang))))

(defun eg--ask-for-language (prompt)
  "Ask for language in PROMPT."
  (eg--completing-read-sexp prompt (eg--get-languages)))

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
  (let* ((examples (eg--get-examples fn))
         (updated-examples (remove example examples)))
    (eg--update-examples fn updated-examples)
    (not (equal updated-examples examples))))

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

(defun eg-get-examples-specify-language ()
  (interactive)
  (let* ((lang (eg--ask-for-language "Which language to fetch examples from: "))
         (fn (eg--ask-for-function "Which function to fetch examples from: " lang)))
    (message "%s" (eg--get-examples fn
                                    lang))))

(defun eg-modify-example (fn-prompt get-example perform-function success-prompt fail-prompt)
  "PERFORM-FUNCTION takes example and fn to modify eg-examples. This function is used as template for eg-add-example and eg-remove-example. GET-EXAMPLE takes in fn."
  (let* ((fn (eg--ask-for-function fn-prompt))
         (example (funcall get-example fn)))
    (if (funcall perform-function example fn)
        (message success-prompt example fn)
      (message fail-prompt example fn)))
  (when eg-save-on-every-update
    (eg-save-examples)))

(cl-defun eg-print-examples (&optional (fn eg--operator))
  "Print examples associated with FN. If FN is nil, set FN to operator of current list."
  (interactive)
  (message (eg--examples-to-string fn)))

(defun eg-visit-examples ()
  (interactive)
  (find-file eg-file))

(define-prefix-command 'eg-command-map)
(global-set-key (kbd "C-c C-e") 'eg-command-map)

(general-def
  :keymaps 'lispy-mode-map
  "C-+" 'eg-run-examples)

(general-def
  :prefix "C-c C-e"
  :keymaps 'lispy-mode-map
  "C-a" 'eg-add-example
  "C-r" 'eg-run-examples
  "C-s" 'eg-save-examples
  "S-r" 'eg-remove-examples
  "C-S-r" 'eg-remove-examples
  )

(general-def
  :keymaps '(emacs-lisp-mode-map eshell-mode-map)
  "C-c C-z" 'eshell-toggle
  )

(load (expand-file-name "~/eg/eg-live.el"))
