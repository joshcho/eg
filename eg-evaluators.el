(require 'haskell-mode)
(defmacro eg-eval-haskell (expr)
  "Evaluate EXPR in Haskell."
  (let ((res (gensym)))
    `(let (,res)
       (let ((session (haskell-interactive-session))
             (process (haskell-interactive-process)))
         (haskell-process-queue-command
          process
          (make-haskell-command
           :state (list session process ,expr 0)
           :go (lambda (state)
                 (haskell-process-send-string (cadr state)
                                              (haskell-interactive-mode-multi-line (cl-caddr state)))
                 (haskell-process-set-evaluating (cadr state) t))
           :complete
           (lambda (state response)
             (haskell-process-set-evaluating (cadr state) nil)
             (setq ,res response)
             ))))
       (while (null ,res)
         (sleep-for .001))
       (if (cl-search "error:" ,res)
           (error ,res)
         (if (< 0 (length ,res))
             (substring ,res 0 -1)
           ,res)))))
