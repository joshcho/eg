;; eg.el -- Access examples on the fly

(require 'lispy)
(require 'crux)
(require 'cl-format)
(defun crux-current-buffer-string ()
  "Get string of current buffer without properties."
  (substring-no-properties (buffer-string)))

;; used in conjunction with lispy--cleanup-overlay
(defmacro lispy--show-inline (expr)
  "Display the result of EXPR inline. This is implemented as a macro as EXPR must be evaluated after the program traverses to the appropriate location."
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

(defun eg-load-examples ()
  (interactive)
  (setq eg-examples (read (file-to-string (expand-file-name "~/eg/eg-examples.el")))))
(eg-load-examples)

(defun eg--current-list ()
  "Get current list. FIXME: Not a perfect solution for handling different positions cursor could be in."
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

(defun eg--operator (expr)
  "Returns the operator in EXPR."
  (when expr
    (when (equal (first expr) 'quote)
      (setq expr (second expr)))
    (if (eg--def-p expr)
        (second expr)
      (first expr))))

(defun eg--args (expr)
  "Returns the args in EXPR."
  (when expr
    (when (equal (first expr) 'quote)
      (setq expr (second expr)))
    (if (eg--def-p expr)
        (third expr)
      (rest expr))))

(defun eg--get-examples (fn)
  "Get examples associated with FN in eg-examples."
  (mapcar (lambda (s) (cons fn s)) (cdr (assoc fn eg-examples))))

(defun eg--update-examples (fn examples)
  "Set examples associated with FN to EXAMPLES."
  (if examples
      (setf (cdr (assoc fn eg-examples))
            (mapcar #'cdr examples))
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
                     " ")
      (format "No examples associated with %s."
              (if eg-print-functions-uppercase
                  (upcase (prin1-to-string fn))
                fn)))))

(defun eg--perform-add-example (example fn)
  "Add EXAMPLE to FN."
  (unless (member example (eg--get-examples fn))
    (eg--update-examples fn (append (eg--get-examples fn)
                                    (list example)))))

(defun eg--parse-args (operands)
  "Parse arguments for OPERANDS."
  (remove-if #'listp
             (cl-loop for o in operands
                      unless (member o '(&optional))
                      collect o)))

;; (defvar eg-add-complete-p t
;;   "FIXME: Support later.

;; Complete function when adding example.")
;; (defvar eg-add-complete-args-p nil
;;   "FIXME: Support later.
;; Complete arguments when adding example. For this to be true, eg-add-complete-p must be true as well.")

(defun eg-add-example (&optional example fn)
  (interactive)
  (unless fn (setq fn (eg--operator (eg--current-list))))
  (setq fn (eg--completing-read-sexp "Associated function for adding example: " (eg--get-functions) fn))
  (setq args (eg--parse-args (eg--args (eg--current-list)))) ; weird bug where args must be setq outside of cl-format
  (setq prompt-string
        (if eg-add-complete-p
            (cl-format nil "Example to add: ")
          (cl-format nil "Example to add for ~a: " fn)))
  (setq completion-string
        (cond
         ((and (not eg-add-complete-p) eg-add-complete-args-p)
          (error "eg-add-complete-p must be true for eg-add-complete-args-p to be true."))
         ((and eg-add-complete-p eg-add-complete-args-p)
          (cl-format nil "(~a ~{~a~^ ~})" fn args)
          )
         ((and eg-add-complete-p (not eg-add-complete-args-p))
          (cl-format nil "(~a)" fn))
         (t "")))
  (unless example (setq example (read (read-from-minibuffer prompt-string
                                                            completion-string))))
  (if (eg--perform-add-example example fn)
      (message "Added %s to %s" example fn)
    (message "%s already an example in %s" example fn)))

(defun eg--perform-remove-example (fn example)
  "Remove EXAMPLE from FN. Returns t if removed, nil otherwise."
  (let ((examples (eg--get-examples fn)))
    (eg--update-examples fn (remove example (eg--get-examples fn)))
    (not (equal examples (eg--get-examples fn)))))

(defvar eg-complete-function-p nil)
(defun eg--completing-read-sexp (prompt collection initial-sexp)
  (setq fn (read (completing-read prompt
                                  (mapcar #'prin1-to-string
                                          collection)
                                  nil nil
                                  (when eg-complete-function-p
                                    (prin1-to-string initial-sexp))
                                  ))))

(defun eg-remove-example (&optional fn example)
  "Remove EXAMPLE from FN."
  (interactive)
  (unless fn (setq fn (eg--operator (eg--current-list))))
  (setq fn (eg--completing-read-sexp "Associated function for example removal: " (eg--get-functions) fn))
  (if (not (eg--get-examples fn))
      (message "No examples associated with %s." fn)
    (progn
      (unless example
        (setq example
              (eg--completing-read-sexp "Example to remove:" (eg--get-examples fn) nil)))
      (if (eg--perform-remove-example fn example)
          (message "Removed %s from %s" example fn)
        (message "Could not find example %s in %s" example fn)))))


(eg--perform-remove-example 'expt '(expt 3 4))

(defun eg-print-examples (&optional fn)
  "Print examples associated with FN. If FN is nil, set FN to operator of current list."
  (interactive)
  (unless fn (setq fn (eg--operator (eg--current-list))))
  (message (eg--examples-to-string fn))
  )

(defun eg-edit-cases (&optional filename)
  "Edit cases corresponding to current operator in FILENAME. FILENAME defaults to eg-file."
  (interactive)
  (unless filename (setq filename eg-file))
  (let ((query (eg--op))
        (args (eg--args)))
    (setq query (symbol-name query))
    (load filename)
    (lispy--show-inline (assoc eg-examples query))
    ;; (when (eql major-mode 'fundamental-mode)
    ;;   (cond ((string-match ".el" (buffer-file-name)) (emacs-lisp-mode))
    ;;         ((string-match ".lisp" (buffer-file-name)) (lisp-mode))))
    ;; (crux-current-buffer-string)
    ;; (beginning-of-buffer)
    ;; (unless (re-search-forward (concat ";; _" query ".*\n") nil t)
    ;;   (end-of-buffer)
    ;;   (insert "\n\n")
    ;;   (insert ";; _" query "\n")
    ;;   (insert "(" query (string-join (mapcar (lambda (s) (format "%s" s)) (when args (cons "" args))) " ") ")")
    ;;   )
    ;; (while (not (current-line-empty-p))
    ;;   (next-line))
    ))

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun eg-return-to-function ()
  "Edit cases corresponding to current operator."
  (interactive)
  (when (re-search-backward ";; _\\([a-zA-Z0-9-%&*]+\\).*\n" nil t)
    (let ((function-name (match-string 1)))
      (xref-find-definitions-other-window function-name))))

(defun eg-function-cases ()
  "Flash between function and its cases. FIXME: string-match isn't perfect right now."
  (interactive)
  (if (string-match "\\(~eg\\)" (buffer-file-name))
      (eg-return-to-function)
    (eg-edit-cases)))

(define-key emacs-lisp-mode-map (kbd "C-+") 'eg-function-cases)
(define-key lisp-mode-map (kbd "C-+") 'eg-between-function-cases)
