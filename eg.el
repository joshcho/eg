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
;; 1. Weird behavior when calling run-examples (probably) from eg-live-fn buffer
;; 2. Mark set when eg-live or eg-live-fn?
;; 3. Watch for performance with benchmark
;; 4. Adjoin doesn't work properly with function selection completing-read
;; 5. When adding examples, being at end of sexp doesn't work
;; 6. Lambda nil printing

;; TODO
;; 1. Polish support for python
;; 2. Add expected value? Cached value?
;; 3. Provide an option to evaluate current expression as well when calling eg-run-examples
;; 4. Show recommended function first (instead of last)
;; 5. Add interactive add example (like eg-live)
;; 6. Add multiline support for inline displays
;; 7. Allow persistent commented tests
;; 8. Separate examples for different languages (and load them separately, too)
;; 9. Make lang into keyword, not optional
;; 10. Differentiate between my keybindings and general keybindings (use-package just for myself)
;; 11. Fix some porting issues in README (consider markdown)
;; 12. Consider orthogonalizing further as needed by python integration
;; 13. Add truncate options for long results
;; 14. Figure out &optional and langs (probably with global variable eg-live-fn-lang or something)
;; 15. Better name than eg?
;; 16. Customize format in *eg-live*, list, smart, expressions, etc.
;; 17. Async?

;; TODO for python support
;; 1. Integrate with lsp?

;; TODO for eg-live
;; 1. Display header instead of comment
;; 2. Think of better names for eg-master and eg-live
;; 3. *Live preview of examples as you scroll through, maybe use consult?*
;; 4. Generalize personal use with exit-emacs-state
;; 5. Better display of non-lisp examples
;; 6. Maybe take inspiration from magit?
;; 7. Add better support for python
;; 8. Sync when window is deleted, but don't close (undo doesn't work otherwise). When closing, try to sync.
;; 9. eg-live should add current example maybe when no examples exist

;; THUNKS
;; 1. Take a look at math/scratch.lisp. Maybe more interactive ways of modifying content?

;;; Code:

(require 'lispy)
(require 'thingatpt)
(require 'ht)

(defgroup eg nil
  "Configuration for eg."
  :prefix "eg-"
  :group 'convenience)

(defun eg--file-to-string (file)
  "Get FILE to string function."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defmacro lispy--show-inline (expr)
  "Display the result of EXPR inline. EXPR must evaluate to string. This is implemented as a macro as EXPR must be evaluated after the program traverses to the appropriate location. FIXME: Really weird interaction."
  `(save-excursion
     (unless (eq major-mode 'python-mode)
       (lispy--back-to-paren))
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
             ((eq major-mode 'python-mode)
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
  "Convert LOCAL-EXAMPLES to stored format. See 'eg--stored->local' for explanation."
  (cl-loop for lang being the hash-keys in local-examples
           using (hash-value fn-examples)
           collect (cons lang (ht->alist fn-examples))
           ))

(defun eg--stored->local (stored-examples)
  "Convert STORED-EXAMPLES to local format. Note that eg use two representations for data. When storing examples in 'eg-file', eg use nested association list. When loading and using examples in Emacs, eg use nested hash table. This function (and 'eg--local->stored') convert between the two formats."
  (ht<-alist
   (cl-loop for (lang . fn-examples) in stored-examples
            collect (cons lang (ht<-alist fn-examples)))))

(defun eg--local-examples-equal (examples1 examples2)
  "Check if EXAMPLES1 and EXAMPLES2 are equal to each other in local format (i.e. format of 'eg-examples')."
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
  "Load 'eg-examples' from `eg-file'."
  (interactive)
  (let ((stored-examples (read (eg--file-to-string eg-file))))
    (setq eg-examples (eg--stored->local stored-examples)))
  ;; (eg--prettify-eg-examples)
  )

(defun eg--sort-stored (examples)
  "Sort EXAMPLES."
  (cl-sort (cl-loop for (lang . fn-examples) in examples
                    collect (cons lang (cl-sort fn-examples #'string< :key #'car)))
           #'string< :key #'car))

(defvar eg-sort-on-save t
  "Whether to sort your examples on save. Can be slow if you have a lot of examples.")
(defun eg--examples-modified ()
  "Check if `eg-examples' is modified since load."
  (not (eg--local-examples-equal
        eg-examples
        (eg--stored->local (read (eg--file-to-string eg-file))))))

(defun eg-save-examples ()
  "Save current state of `eg-examples' to your `eg-file'."
  (interactive)
  (if (eg--examples-modified)
      (save-window-excursion
        (let ((buffer-already-open (get-file-buffer eg-file)))
          (find-file eg-file)
          (erase-buffer)
          (insert ";; " eg-examples-doc "\n")
          (insert (prin1-to-string
                   (funcall (if eg-sort-on-save
                                #'eg--sort-stored
                              #'identity)
                            (eg--local->stored eg-examples))))
          (lispy-multiline) ; FIXME: get rid of this dependency if possible
          (insert ")")
          (save-buffer)
          (unless buffer-already-open
            (kill-buffer))))
    (message "Examples not modified since load, skip saving.")))

(defvar eg-save-on-every-update nil
  "If t, then eg saves every time add or remove occurs. Otherwise, you have to save current state of examples manually through `eg-save-examples'.")
(defvar eg-ask-save-on-exit t
  "If t, ask to save on exiting Emacs.")

(add-hook 'kill-emacs-hook (lambda () (when (and eg-ask-save-on-exit
                                            (eg--examples-modified)
                                            (yes-or-no-p (format "eg: Save your examples to %s?" eg-file)))
                                   (eg-save-examples))))

(defvar eg-load-on-startup t)
(when eg-load-on-startup
  (eg-load-examples))

(defun eg--current-list ()
  "Get current list around point. FIXME: Not a perfect solution for handling different positions cursor could be in."
  (if (member major-mode '(lisp-mode emacs-lisp-mode))
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
          nil))
    (thing-at-point 'line)))

(defun eg--current-lang ()
  "Get current language of buffer."
  (cl-case major-mode
    ('emacs-lisp-mode 'emacs-lisp)
    ('lisp-mode 'lisp)
    ('python-mode 'python)))

(defun eg--def-p (expr)
  "Check if EXPR is def expression (e.g. defun, defmacro, defgeneric)."
  (member (first expr) '(cl-defun cl-defmacro defun defmacro defgeneric)))

(cl-defun eg--operator ()
  "Return the operator in current form."
  (let ((expr (eg--current-list))
        (lang (eg--current-lang)))
    (if (equal lang 'python)
        (intern (lispy--current-function))
      (when expr
        (when (equal (first expr) 'quote)
          (setq expr (second expr)))
        (if (eg--def-p expr)
            (second expr)
          (first expr))))))

;; (cl-defun eg--args (&optional (expr (eg--current-list)))
;;   "Returns the args in EXPR."
;;   (when expr
;;     (when (equal (first expr) 'quote)
;;       (setq expr (second expr)))
;;     (if (eg--def-p expr)
;;         (third expr)
;;       (rest expr))))

;; (cl-defun eg--example-template (&optional (expr (eg--current-list)))
;;   (cons (eg--operator expr) (eg--args expr)))

(cl-defun eg--get-examples (fn &optional (lang (eg--current-lang)))
  "Get examples associated with LANG and FN in `eg-examples'."
  (ht-get* eg-examples lang fn))
(cl-defun eg--update-examples (fn examples &optional (lang (eg--current-lang)))
  "Set examples associated with LANG and FN to EXAMPLES."
  (if (null examples)
      (ht-remove (ht-get eg-examples lang) fn)
    (setf (ht-get* eg-examples lang fn) examples)))

(cl-defun eg--get-functions (&optional (lang (eg--current-lang)))
  "Get a list of functions associated with LANG."
  (if-let (lang-hash (ht-get eg-examples lang))
      (ht-keys lang-hash)
    (progn
      (ht-set! eg-examples lang (ht-create))
      nil)))

(cl-defun eg--get-languages ()
  "Get languages for which we have examples in 'eg-examples'."
  (ht-keys eg-examples))

(defvar eg-print-functions-uppercase nil
  "Whether to print functions in upper case.")

(cl-defun eg--print-example (example &optional (lang (eg--current-lang)))
  "Print EXAMPLE given LANG."
  (if (member lang lisp-languages)
      (prin1-to-string example)
    example))

(cl-defun eg--print-examples (examples &optional (lang (eg--current-lang)))
  "Print EXAMPLES given LANG."
  (string-join (mapcar #'(lambda (e) (eg--print-example e lang)) examples)
               "\n"))

(defvar lisp-languages '(lisp emacs-lisp))

(cl-defun eg--examples-to-string (fn &optional (print-examples-function #'eg--print-examples) (lang (eg--current-lang)))
  "Get string of examples associated with LANG and FN using PRINT-EXAMPLES-FUNCTION."
  (let ((examples (eg--get-examples fn lang)))
    (if (null examples)
        (format "No examples associated with %s."
                (funcall (if eg-print-functions-uppercase
                             (-compose #'upcase #'prin1-to-string)
                           #'identity)
                         fn))
      (funcall print-examples-function examples))))
(defvar eg-align-test-results t)
(cl-defun eg--run-examples-to-string (fn &optional (lang (eg--current-lang)))
  "Get string of examples associated with LANG and FN with run."
  (eg--examples-to-string fn #'(lambda (examples)
                                 (string-join
                                  (eg--align-strings
                                   (mapcar
                                    (lambda (e)
                                      (let* ((example-string (eg--print-example e lang))
                                             (eval-result
                                              (condition-case nil
                                                  (lispy--eval example-string)
                                                (error "Eval error"))))
                                        (if (string-empty-p eval-result)
                                            example-string
                                          (concat example-string
                                                  " => "
                                                  eval-result))))
                                    examples)
                                   "=>")
                                  "\n"))))

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

(defun eg-show-examples-prompt ()
  "Prompt for a function. Show examples associated with that function."
  (interactive)
  (let ((str (eg--examples-to-string
              (eg--completing-read-operator "Function to show examples for: " (eg--get-functions)))))
    (lispy--show-inline str)))

(defun eg-run-examples-prompt ()
  "Prompt for a function. Run and show examples associated with that function."
  (interactive)
  (let ((str (eg--run-examples-to-string
              (eg--completing-read-operator "Function to show examples for: " (eg--get-functions)))))
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

(defun eg--completing-read-operator (prompt collection)
  "Do a `completing-read' with PROMPT on COLLECTION."
  (funcall (if (member (eg--current-lang) lisp-languages)
               #'read
             #'intern)
           (completing-read prompt
                            (mapcar #'prin1-to-string
                                    collection))))

(defun eg--read-from-minibuffer-example (prompt initial-value)
  "Do a `read-from-minibuffer' with PROMPT with INITIAL-VALUE."
  (funcall (if (member (eg--current-lang) lisp-languages)
               #'read
             #'identity)
           (read-from-minibuffer prompt
                                 initial-value)))

(cl-defun eg--ask-for-function (prompt &optional (lang (eg--current-lang)))
  "Ask for function in PROMPT given LANG."
  (eg--completing-read-operator prompt (append (when-let (op (eg--operator))
                                                 (list op))
                                               (eg--get-functions lang))))

(defun eg--ask-for-language (prompt)
  "Ask for language in PROMPT."
  (eg--completing-read-operator prompt (eg--get-languages)))

(defun eg--perform-add-example (example fn)
  "Add EXAMPLE to FN."
  (unless (member example (eg--get-examples fn))
    (eg--update-examples fn (append (eg--get-examples fn)
                                    (list example)))))

(defun eg--query-example-for-add (fn)
  "Query example for FN."
  (eg--read-from-minibuffer-example "Example to add: "
                                    (when (member (eg--current-lang) '(lisp emacs-lisp))
                                      (when (equal (first (eg--current-list)) fn)
                                        (prin1-to-string (eg--current-list))))))

(defun eg-add-example ()
  "Interactive add."
  (interactive)
  (eg-modify-example "Associated function: "
                     #'eg--query-example-for-add
                     #'eg--perform-add-example
                     "Added %s to %s"
                     "%s already an example in %s"))

(defun eg--perform-remove-example (example fn)
  "Remove EXAMPLE from FN. Return t if removed, nil otherwise."
  (let* ((examples (eg--get-examples fn))
         (updated-examples (remove example examples)))
    (eg--update-examples fn updated-examples)
    (not (equal updated-examples examples))))

(defun eg--query-example-for-remove (fn)
  "Query EXAMPLE to remove for FN."
  (eg--completing-read-operator "Example to remove: "
                                (if (equal (eg--operator) fn)
                                    (adjoin (eg--operator) (eg--get-examples fn))
                                  (eg--get-examples fn))))

(defun eg-remove-example ()
  "Interactive removal of examples."
  (interactive)
  (eg-modify-example "Associated function: "
                     #'eg--query-example-for-remove
                     #'eg--perform-remove-example
                     "Removed %s from %s"
                     "Could not find example %s in %s"))

(defun eg-get-examples ()
  "Interactive get of examples."
  (interactive)
  (message "%s" (eg--get-examples (eg--ask-for-function "Which function to fetch examples from: "))))

(defun eg-get-examples-specify-language ()
  "Interactive get of examples based on language."
  (interactive)
  (let* ((lang (eg--ask-for-language "Which language to fetch examples from: "))
         (fn (eg--ask-for-function "Which function to fetch examples from: " lang)))
    (message "%s" (eg--get-examples fn
                                    lang))))

(defun eg-modify-example (fn-prompt get-example perform-function success-prompt fail-prompt)
  "PERFORM-FUNCTION takes example and fn to modify `eg-examples'. This function is used as template for `eg-add-example' and `eg-remove-example'. GET-EXAMPLE takes in fn. Use FN-PROMPT, SUCCESS-PROMPT, and FAIL-PROMPT for interactive queries."
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
  "Visit 'eg-file'."
  (interactive)
  (find-file eg-file))

(define-prefix-command 'eg-command-map)
(global-set-key (kbd "C-c C-e") 'eg-command-map)

(general-def
  :keymaps '(lisp-mode-map emacs-lisp-mode-map python-mode-map)
  "C-+" 'eg-run-examples)

(general-def
  :keymaps 'python-mode-map
  "C-c C-e" nil)

(general-def
  :prefix "C-c C-e"
  :keymaps '(lisp-mode-map emacs-lisp-mode-map python-mode-map)
  "C-a" 'eg-add-example
  "C-r" 'eg-run-examples
  "C-s" 'eg-save-examples
  "C-l" 'eg-load-examples
  "S-r" 'eg-remove-examples
  "C-S-r" 'eg-remove-examples
  )

(general-def
  :keymaps '(emacs-lisp-mode-map eshell-mode-map)
  "C-c C-z" 'eshell-toggle
  )

(load (expand-file-name "~/eg/eg-live.el"))

(provide 'eg)

;;; eg.el ends here
