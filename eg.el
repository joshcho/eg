;;; eg.el --- Access examples (e.g. eg) on the fly -*- lexical-binding: t -*-
;;
;; Copyright © 2022-2022 Jungmin "Josh" Cho
;;
;; Author: Josh Cho <joshchonpc@gmail.com>
;; URL: https://github.com/joshcho/eg
;; Version: alpha
;; Keywords: convenience
;; Package-Requires: (lispy ht anaphora dash)

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
;; 3. There are some performance issues after long use? Memory issues maybe
;; 4. New example broken when string is in arg
;; 5. Operator broken when in comment

;; TODO
;; 1. Polish support for python
;; 2. Provide an option to evaluate current expression as well when calling eg-run-examples
;; 3. Allow persistent commented tests
;; 4. Differentiate between my keybindings and general keybindings (use-package just for myself)
;; 5. Better name than eg?
;; 6. Add lispy-try support
;; 7. Remove eg-master
;; 8. Use text instead of s-expressions for speed and portability
;; 9. Enable multiline expressions
;; 10. Add defcustoms

;; TODO for python support
;; 1. (setq python-indent-guess-indent-offset-verbose nil) is needed?

;; TODO for eg-live
;; 1. Think of better names for eg-master and eg-live
;; 2. *Live preview of examples as you scroll through, maybe use consult?*
;; 3. Maybe take inspiration from magit?
;; 4. Sync when window is deleted, but don't close (undo doesn't work otherwise). When closing, try to sync.

;;; Code:

(require 'lispy)
(require 'thingatpt)
(require 'ht)
(require 'anaphora)
(require 'dash)

(defgroup eg nil
  "Configuration for eg."
  :prefix "eg-"
  :group 'convenience)

(defun eg--file-to-string (file)
  "Get FILE to string function."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defvar eg-file (expand-file-name "~/eg/eg-examples.el"))

(defvar eg-examples nil
  "Examples associated with functions.")

(defun eg--local->stored (local-examples)
  "Convert LOCAL-EXAMPLES to stored format. See 'eg--stored->local' for explanation."
  (cl-loop for mode being the hash-keys in local-examples
           using (hash-value fn-examples)
           collect (cons mode (ht->alist fn-examples))
           ))

(defun eg--stored->local (stored-examples)
  "Convert STORED-EXAMPLES to local format. Note that eg use two representations for data. When storing examples in 'eg-file', eg use nested association list. When loading and using examples in Emacs, eg use nested hash table. This function (and 'eg--local->stored') convert between the two formats."
  (ht<-alist
   (cl-loop for (mode . fn-examples) in stored-examples
            collect (cons mode (ht<-alist fn-examples)))))

(defun eg--local-examples-equal (examples1 examples2)
  "Check if EXAMPLES1 and EXAMPLES2 are equal to each other in local format (i.e. format of 'eg-examples')."
  (and
   (= (ht-size examples1)
      (ht-size examples2))
   (cl-loop for mode being the hash-keys in examples1
            always (ht-equal? (ht-get examples1 mode)
                              (ht-get examples2 mode)))))

(defun eg-load-examples ()
  "Load 'eg-examples' from 'eg-file'."
  (interactive)
  (let ((stored-examples (read (eg--file-to-string eg-file))))
    (setq eg-examples (eg--stored->local stored-examples)))
  )

(defun eg--sort-stored (examples)
  "Sort EXAMPLES."
  (cl-sort (cl-loop for (mode . mode-examples) in examples
                    collect (cons mode (cl-sort mode-examples #'string< :key #'car)))
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
      (with-temp-file eg-file
        (insert ";; eg-examples. changes made by hand may be overwritten. if you made changes, make sure to call eg-load-examples to sync eg-examples with your file.\n")
        (insert (prin1-to-string
                 (funcall (if eg-sort-on-save
                              #'eg--sort-stored
                            #'identity)
                          (eg--local->stored eg-examples))))
        (lispy-multiline)
        (message "Examples saved to %s" eg-file))
    (message "Examples not modified since last load, skip saving")))

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
  "Get current list around point. FIXME: different types returned."
  (if (member major-mode '(lisp-mode emacs-lisp-mode))
      (the list
        (save-excursion
          (cond
           ((or (nth 3 (syntax-ppss))
                (nth 4 (syntax-ppss)))
            ;; comment or string
            (goto-char (nth 1 (syntax-ppss)))
            (forward-char))
           ((eq ?\'
                (char-after))
            (forward-char 2))
           ((eq ?\(
                (char-after))
            (forward-char))
           ((eq ?\)
                (char-before))
            (backward-char))
           ((zerop (nth 0 (syntax-ppss)))
            ;; depth in parens is zero
            '())
           (t
            (goto-char (nth 1 (syntax-ppss)))
            (forward-char)))
          (list-at-point)))
    (the string
      (thing-at-point 'line))))

(defun lispy--python-end-of-object ()
  (save-excursion
    (if (bolp)
        (forward-char)
      (forward-sexp))
    (while (not (or
                 (eolp)
                 (looking-at "[[ \t(]")))
      (forward-sexp))
    (point)))

(defun eg--operator ()
  "Return `appropriate` function name. If in a def- expression, return the function being defined. Otherwise, return the function of the current list."
  (the symbol
    (if (member major-mode eg-lisp-modes)
        (let ((expr (eg--current-list)))
          (when expr
            (when (equal (first expr) 'quote)
              (setq expr (second expr)))
            (if (member (first expr) '(cl-defun cl-defmacro defun defmacro defgeneric))
                (second expr)
              (first expr))))
      (awhen (thing-at-point 'symbol)
        (cond ((save-excursion
                 (beginning-of-thing 'symbol)
                 (eql (char-before) ?.))
               (intern (buffer-substring (lispy--python-beginning-of-object) (cdr (bounds-of-thing-at-point 'symbol)))))
              ((save-excursion
                 (end-of-thing 'symbol)
                 (eql (char-after) ?.))
               (intern (buffer-substring (car (bounds-of-thing-at-point 'symbol)) (lispy--python-end-of-object))))
              (t
               (intern it)))))))

(cl-defun eg--get-examples (fn)
  "Get examples associated with FN in `eg-examples'."
  (ht-get* eg-examples major-mode fn))

(cl-defun eg--set-examples (fn examples)
  "Set examples associated with FN to EXAMPLES."
  (if (null examples)
      (ht-remove (ht-get eg-examples major-mode) fn)
    (setf (ht-get* eg-examples major-mode fn) examples)))

(cl-defun eg--get-functions ()
  "Get a list of functions."
  (if-let (hash (ht-get eg-examples major-mode))
      (ht-keys hash)
    (progn
      (ht-set! eg-examples major-mode (ht-create))
      nil)))

(defun eg--ask-for-function (prompt)
  "Ask for function in PROMPT."
  (let ((function-list (eg--get-functions))
        (op (eg--operator)))
    (the symbol
      (read (completing-read prompt
                             (if op
                                 (adjoin op function-list)
                               function-list)
                             nil nil nil nil
                             (when op
                               (if (eql major-mode 'python-mode)
                                   (s-replace "\\" "" (prin1-to-string op))
                                 (prin1-to-string op))))))))

(define-prefix-command 'eg-command-map)
(global-set-key (kbd "C-c C-e") 'eg-command-map)

(general-def
  :keymaps 'python-mode-map
  "C-c C-e" nil)

(general-def
  :prefix "C-c C-e"
  :keymaps 'global-map
  "C-s" 'eg-save-examples
  "C-l" 'eg-load-examples
  )

(general-def
  :keymaps '(emacs-lisp-mode-map eshell-mode-map)
  "C-c C-z" 'eshell-toggle
  )

(load (expand-file-name "~/eg/eg-live.el"))
(load (expand-file-name "~/eg/eg-deferred.el"))

(provide 'eg)

;;; eg.el ends here
