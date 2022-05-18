;; eg-examples. changes made by hand may be overwritten. if you made changes, make sure to call eg-load-examples to sync eg-examples with your file.
((completing-read ("Letter: " '(a b c d)))
 (format (";; %s" 'a))
 (+ (1 2)
    (2 3)
    (3 3)
    (4 5))
 (app-define-key ("Emacs" "C-@" "@"))
 (define-prefix-command ('eg-command-map))
 (eg--args ('(+ 1 2))
           ('(defun square (x)
               (* x x))))
 (eg--current-list nil)
 (eg--def-p ('(defun square (x)
                (* x x)))
            ('(square 2)))
 (eg--example-template ('(+ 1 2))
                       ('(defun square (x)
                           (* x x))))
 (eg--examples-to-string (#'+))
 (eg--get-examples ('+))
 (eg--get-functions nil)
 (eg--operator ('(+ 1 2))
               ('(defun square (x)
                   (* x x))))
 (eg--sort-examples ('((a 1)
                       (c 3)
                       (b 2))))
 (expand-file-name ("~/eg/eg.el"))
 (expt (2 3)
       (3 4)
       (5 7)
       (1 2))
 (general-def (:prefix "C-c C-e" :keymaps 'lispy-mode-map "a" 'eg-add-example "C-a" 'eg-add-example "r" 'eg-run-examples "C-r" 'eg-run-examples "S-r" 'eg-remove-examples "C-S-r" 'eg-remove-examples))
 (global-set-key ((kbd "C-c C-e")
                  'eg-command-map))
 (make-test ('(+ 1 2)))
 (square (2))
 (window-by-number (0)))
