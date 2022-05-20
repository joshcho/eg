;; eg-examples. changes made by hand may be overwritten. if you made changes, make sure to call eg-load-examples to sync eg-examples with your file.
((emacs-lisp (+ (+ 1 2)
                (+ 2 3)
                (+ 3 3)
                (+ 4 5)
                (+ 5 7))
             (async-start (let ((proc (async-start (lambda nil (sleep-for 0.1)
                                                     5))))
                            (async-get proc)))
             (completing-read (completing-read "Letter: " '(a b c d)))
             (concat (benchmark-elapse (concat "abcd" "
"))
                     (benchmark-elapse (format "%s
" "abcd")))
             (define-prefix-command (define-prefix-command 'eg-command-map))
             (eg--align-strings (eg--align-strings '("c =>" "contents =>" "htn")
                                                   "=>"))
             (eg--args (eg--args '(+ 1 2))
                       (eg--args '(defun square (x)
                                    (* x x))))
             (eg--buffer-live-p (eg--buffer-live-p))
             (eg--current-list (eg--current-list))
             (eg--def-p (eg--def-p '(defun square (x)
                                      (* x x)))
                        (eg--def-p '(square 2)))
             (eg--example-template (eg--example-template '(+ 1 2))
                                   (eg--example-template '(defun square (x)
                                                            (* x x))))
             (eg--examples-to-string (eg--examples-to-string #'+))
             (eg--get-examples (eg--get-examples '+))
             (eg--get-functions (eg--get-functions))
             (eg--mapcar-middle (eg--mapcar-middle #'1+ '(1 2 3)))
             (eg--mapcar-subseq (eg--mapcar-subseq #'1+ '(1 2 3 4)
                                                   1 2))
             (eg--operator (eg--operator))
             (eg--perform-add-example (eg--perform-remove-example '(+ 5 7)
                                                                  '+)
                                      (eg--perform-add-example '(+ 5 7)
                                                               '+))
             (eg--print-example-strings a b c d)
             (eg--run-examples-to-string (eg--run-examples-to-string #'+))
             (eg--sort-examples (eg--sort-examples '((a 1)
                                                     (c 3)
                                                     (b 2))))
             (eg--sort-stored (eg--sort-stored (eg--local->stored eg-examples)))
             (eg-live-buffer (benchmark-elapse (eg-live-buffer #'+))
                             (eg-live-kill)
                             (benchmark-elapse (eg-live-buffer #'+)))
             (eg-live-populate-examples (benchmark-elapse (eg-live-populate-examples #'+))
                                        (benchmark-elapse (eg-live-populate-examples #'expt)))
             (eg-load-examples (require 'benchmark)
                               (benchmark-elapse (eg-load-examples)))
             (eg-save-examples (require 'benchmark)
                               (benchmark-elapse (eg-save-examples)))
             (expand-file-name (expand-file-name "~/eg/eg.el"))
             (expt (expt 2 3)
                   (expt 3 4)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7)
                   (expt 5 7))
             (format (format ";; %s" 'a))
             (funcall-when (funcall-when t (lambda (s)
                                             (+ s 2))
                                         1)
                           (funcall-when nil (lambda (s)
                                               (+ s 2))
                                         1))
             (general-def (general-def :prefix "C-c C-e" :keymaps 'lispy-mode-map "a" 'eg-add-example "C-a" 'eg-add-example "r" 'eg-run-examples "C-r" 'eg-run-examples "S-r" 'eg-remove-examples "C-S-r" 'eg-remove-examples))
             (global-set-key (global-set-key (kbd "C-c C-e")
                                             'eg-command-map))
             (length (length '(a b c d)))
             (with-eg-live (eg-live-kill)
                           (benchmark-elapse (with-eg-live (with-eg-live (message "hi"))))
                           (eg-live-kill)
                           (benchmark-elapse (with-eg-live (message "hi")))))
 (lisp (access (access '((a . b)
                         (c . d))
                       'a))
       (accesses (accesses *current-remap* "Emacs" "C-@"))
       (adjoin (adjoin 'a '(a b c d)))
       (app-define-key (app-define-key "emacs" "C-@" "@"))
       (function-name (function-name #'+))
       (make-hash-table (make-hash-table :test #'equal))
       (make-test (make-test '(+ 1 2)))
       (remove (remove "a" '(("a" . b)
                             ("c" . d))
                       :key #'car :test #'equal))
       (window-by-number (window-by-number 0)))
 (python (len "len([1,2,3])")
         (max "max(range(4))")
         (range "list(range(1,4))")
         (set "set1 = {1,2}" "set1.add(4)" "set1.remove(1)" "set1"))))
