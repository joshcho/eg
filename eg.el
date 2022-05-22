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
;; 5. Lambda nil printing

;; TODO
;; 1. Polish support for python
;; 2. Add expected value? Cached value?
;; 3. Provide an option to evaluate current expression as well when calling eg-run-examples
;; 4. Add multiline support for inline displays
;; 5. Allow persistent commented tests
;; 6. Separate examples for different languages (and load them separately, too)
;; 7. Make lang into keyword, not optional
;; 8. Differentiate between my keybindings and general keybindings (use-package just for myself)
;; 9. Fix some porting issues in README (consider markdown)
;; 10. Consider orthogonalizing further as needed by python integration
;; 11. Add truncate options for long results
;; 12. Figure out &optional and langs (probably with global variable eg-live-fn-lang or something)
;; 13. Better name than eg?
;; 14. Customize format in *eg-live*, list, smart, expressions, etc.
;; 15. Async?
;; 16. Add example template as core
;; 17. Example format should be maintained throughout sessions (don't use sexp's as portable format). If using strings, check validity on save.
;; 18. Consider different files for things
;; 19. Add lispy-try support
;; 20. Remove eg-master

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

(defun eg--local->stored (local-examples)
  "Convert LOCAL-EXAMPLES to stored format. See 'eg--stored->local' for explanation."
  (cl-loop for lang being the hash-keys in local-examples
           using (hash-value fn-examples)
           collect (cons lang (ht->alist fn-examples))
           ))
(eg--local->stored eg-examples)

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
      (with-temp-file eg-file
        (insert ";; " eg-examples-doc "\n")
        (insert (prin1-to-string
                 (funcall (if eg-sort-on-save
                              #'eg--sort-stored
                            #'identity)
                          (eg--local->stored eg-examples))))
        (lispy-multiline))
    (message "Examples not modified since last load, skip saving.")))

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
  "Get current list around point."
  (if (member major-mode '(lisp-mode emacs-lisp-mode))
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
          '())
         (t
          (goto-char (nth 1 (syntax-ppss)))
          (forward-char)))
        (list-at-point))
    (thing-at-point 'line)))

(defun eg--current-lang ()
  "Get current language of buffer."
  (cl-case major-mode
    ('emacs-lisp-mode 'emacs-lisp)
    ('lisp-mode 'lisp)
    ('python-mode 'python)))

(defun eg--operator ()
  "Return the operator in current form."
  (let ((expr (eg--current-list))
        (lang (eg--current-lang)))
    (if (equal lang 'python)
        (intern (lispy--current-function))
      (when expr
        (when (equal (first expr) 'quote)
          (setq expr (second expr)))
        (if (member (first expr) '(cl-defun cl-defmacro defun defmacro defgeneric))
            (second expr)
          (first expr))))))

(cl-defun eg--get-examples (fn &optional (lang (eg--current-lang)))
  "Get examples associated with LANG and FN in `eg-examples'."
  (ht-get* eg-examples lang fn))

(cl-defun eg--set-examples (fn examples &optional (lang (eg--current-lang)))
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

(defun eg--ask-for-function (prompt)
  "Ask for function in PROMPT given LANG."
  (let ((function-list (eg--get-functions))
        (op (eg--operator)))
    (read (completing-read prompt
                           (if op
                               (adjoin op function-list)
                             function-list)
                           nil nil nil nil
                           (when op
                             (prin1-to-string op))))))

(define-prefix-command 'eg-command-map)
(global-set-key (kbd "C-c C-e") 'eg-command-map)

(general-def
  :keymaps 'python-mode-map
  "C-c C-e" nil)

(general-def
  :prefix "C-c C-e"
  :keymaps '(lisp-mode-map emacs-lisp-mode-map python-mode-map)
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
