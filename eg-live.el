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
  (eg--setup-buffer eg-live-buffer-name (prin1-to-string (eg--local->stored eg-examples)))
  (emacs-lisp-mode)
  (eg-live-mode))

(defvar eg-live-window-height 16)

(defun eg--sync-live-if-modified ()
  (when (member 'eg-live-mode minor-mode-list)
    (let ((current-examples (eg--stored->local (read (buffer-string)))))
      (unless (eg--local-examples-equal eg-examples current-examples)
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
    (define-key map
                (kbd "C-<tab>")
                'eg-live-fn-switch)
    (define-key map
                (kbd "C-c C-r")
                'eg-live-fn-run-toggle)
    map))

(let (buffer-fn)
  (defun eg-live-fn ()
    (interactive)
    (setq buffer-fn (if (member (eg--operator) (eg--get-functions))
                        (eg--operator)
                      (eg--ask-for-function "Function to Edit: "))) ; TODO: add preview options here
    (eg--setup-buffer eg-live-fn-buffer-name
                      (concat (format ";; %s\n" buffer-fn)
                              (prin1-to-string (eg--get-examples buffer-fn))))
    (eg-live-fn-mode)
    )

  (defun eg-live-fn-run-and-comment ()
    (interactive)
    (let ((examples (read (buffer-string))))
      (erase-buffer)
      (insert (format ";; %s\n" (prin1-to-string buffer-fn)))
      (cl-loop
       initially (insert "(")
       for e in examples
       for i upfrom 0
       do (insert (format "%s%s ; => %s\n" (if (= i 0) "" " ") e (lispy--eval (prin1-to-string e))))
       finally (insert " )"))
      (beginning-of-buffer))
    )

  (let (showing-runs)
    (defun eg-live-fn-run-toggle ()
      (interactive)
      (if showing-runs
          (let ((examples (read (buffer-string))))
            (erase-buffer)
            (insert (format ";; %s\n" (prin1-to-string buffer-fn)))
            (insert (prin1-to-string examples))
            (lispy-multiline)
            (beginning-of-buffer))
        (eg-live-fn-run-and-comment))
      (setq showing-runs (not showing-runs))))

  (defun eg-live-fn-switch ()
    (interactive)
    (eg--sync-live-fn-if-modified)
    (let ((temp-fn (eg--ask-for-function "Function to Edit: ")))
      ;; account for keyboard-exit from eg--ask-for-function, but do not modify buffer-fn yet
      (eg--sync-live-fn-if-modified)
      (setq buffer-fn temp-fn))
    (eg--setup-buffer eg-live-fn-buffer-name
                      (concat (format ";; %s\n" buffer-fn)
                              (prin1-to-string (eg--get-examples buffer-fn))))
    (eg-live-fn-mode))

  (defun eg--sync-live-fn-if-modified ()
    (when buffer-fn
      (let ((current-fn-examples (read (buffer-string))))
        (unless (equal (eg--get-examples buffer-fn) current-fn-examples)
          (eg--update-examples buffer-fn current-fn-examples)
          (message "eg-examples synced for %s" buffer-fn))))))

(defun eg--setup-buffer (buffer-name buffer-string)
  "Create new buffer with BUFFER-NAME and BUFFER-STRING. Subroutine of eg-live-fn and eg-live."
  (get-buffer-create buffer-name)
  (let ((stored-major-mode major-mode)) ; FIXME: Language should be specified in the data structure
    (unless (equal (buffer-name (current-buffer)) eg-live-fn-buffer-name)
      (switch-to-buffer-other-window eg-live-fn-buffer-name)
      (set-window-text-height (get-buffer-window) eg-live-window-height))
    (erase-buffer)
    (cond
     ((equal stored-major-mode 'emacs-lisp-mode) (emacs-lisp-mode))
     ((equal stored-major-mode 'lisp-mode) (lisp-mode))))
  (insert buffer-string)
  (lispy-multiline)
  (beginning-of-buffer)
  (evil-emacs-state)                   ;FIXME: generalize personal use
  )

(general-def
  :keymaps 'lispy-mode-map
  eg-live-toggle-key 'eg-live
  eg-live-fn-toggle-key 'eg-live-fn
  )
