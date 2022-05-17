;; https://www.emacswiki.org/emacs/ModeTutorial

(defvar eg-live-mode-hook nil)
(defvar eg-live-mode-map
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for eg-live-mode")
