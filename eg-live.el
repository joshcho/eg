;;; -*- lexical-binding: t -*-

;; https://nullprogram.com/blog/2013/02/06/

(defvar eg-live-mode-hook nil)
(defvar eg-live-mode-quit-hook nil)    ;FIXME: Use this

;; see org-src-mode
(defvar eg-live-toggle-key (kbd "C-c '"))
(define-minor-mode eg-live-mode
  "Get eg live on."
  :lighter " eg-live"
  :keymap
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    (define-key map eg-live-toggle-key #'(lambda ()
                                           (interactive)
                                           (eg--sync-live-if-modified)
                                           (kill-buffer)
                                           (delete-window)))
    map))

(defvar eg-live-buffer-name "*eg-live*")
(defun eg-live ()
  (interactive)
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

(defun eg--sync-live-if-modified ()
  (when (member 'eg-live-mode minor-mode-list)
    (let ((current-examples (read (buffer-string))))
      (unless (equal eg-examples current-examples)
        (setq eg-examples current-examples)
        (message "eg-examples synced")))))

(defvar eg-live-fn-buffer-name "*eg-live-fn*")
(defvar eg-live-fn-toggle-key (kbd "C-c C-'"))
(define-minor-mode eg-live-fn-mode
  "Get eg live fn on."
  :lighter " eg-live-fn"
  :keymap
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    (define-key map
                eg-live-fn-toggle-key
                #'(lambda ()
                    (interactive)
                    (eg--sync-live-fn-if-modified)
                    (kill-buffer)
                    (delete-window)))
    map))

(let (buffer-fn)
  (defun eg-live-fn ()
    (interactive)
    (get-buffer-create eg-live-fn-buffer-name)
    (switch-to-buffer-other-window eg-live-fn-buffer-name)
    (erase-buffer)
    (emacs-lisp-mode)
    (eg-live-fn-mode)
    (evil-emacs-state)   ; FIXME: generalize personal use
    (setq buffer-fn nil) ; to account for cancelling minibuffer command
    (setq buffer-fn (eg--ask-for-function "Function to Edit: "))
    (insert (format ";; %s\n" buffer-fn)) ; FIXME: make this into a proper header
    (insert (prin1-to-string (eg--get-examples buffer-fn)))
    (lispy-multiline)
    (beginning-of-buffer)
    )

  (defun eg--sync-live-fn-if-modified ()
    (when buffer-fn
      (let ((current-fn-examples (read (buffer-string))))
        (unless (equal (eg--get-examples buffer-fn) current-fn-examples)
          (eg--update-examples buffer-fn current-fn-examples)
          (message "eg-examples synced for %s" buffer-fn))))))

(general-def
  :keymaps 'lispy-mode-map
  eg-live-toggle-key 'eg-live
  eg-live-fn-toggle-key 'eg-live-fn)
