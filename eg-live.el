;; https://nullprogram.com/blog/2013/02/06/


(defvar eg-live-mode-hook nil)
(defvar eg-live-mode-quit-hook nil)
;; (defvar eg-live-mode-map
;;   (let ((map (make-keymap)))
;;     ;; (define-key map "\C-j" 'newline-and-indent)
;;     (define-key map (kbd "C-c '") #'(lambda ()
;;                                       (interactive)
;;                                       (kill-buffer)
;;                                       (delete-window)))
;;     map)
;;   "Keymap for eg-live-mode")

;; see org-src-mode
(define-minor-mode eg-live-mode
  "Get eg live on."
  :lighter " eg-live"
  :keymap
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    (define-key map (kbd "C-c '") #'(lambda ()
                                      (interactive)
                                      (kill-buffer)
                                      (delete-window)))
    map))

(defvar eg-live-buffer-name "*eg-live*")
(defun eg-live ()
  (interactive)
  (setq eg-examples (eg--sort-examples eg-examples))
  (get-buffer-create eg-live-buffer-name)
  (switch-to-buffer-other-window eg-live-buffer-name)
  (erase-buffer)
  (insert (prin1-to-string eg-examples))
  (lispy-multiline)
  (beginning-of-buffer)
  (emacs-lisp-mode)
  (eg-live-mode)
  (evil-emacs-state)                  ; FIXME: generalize personal use
  )

(defun eg--sort-examples (examples)
  (cl-sort examples
           #'string<
           :key #'car))

(defun eg--sync-if-modified ()
  (when (member 'eg-live-mode minor-mode-list)
    (let ((current-examples (read (buffer-string))))
      (unless (equal eg-examples current-examples)
        (setq eg-examples current-examples)
        (message "eg-examples synced")))))

(add-hook 'kill-buffer-hook #'eg--sync-if-modified)

(general-def
  :keymaps 'lispy-mode-map
  "C-c '" 'eg-live)
