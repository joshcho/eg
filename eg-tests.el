(require 'ert)

(ert-deftest conversion-test ()
  (should (eg--local-examples-equal
           (eg--stored->local (eg--local->stored eg-examples))
           eg-examples)))

(ert-deftest modified-test ()
  (should (null
           (progn
             (eg-load-examples)
             (eg--examples-modified)))))

(ert-deftest load-save-test ()
  (should (progn
            (eg-load-examples)
            (let ((temp eg-examples))
              (eg-save-examples)
              (eg-load-examples)
              (eg--local-examples-equal eg-examples temp)))))
