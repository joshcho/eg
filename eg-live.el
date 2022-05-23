;;; eg-live.el --- Implement eg-live and eg-master -*- lexical-binding: t -*-
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

;; Implement eg-master-mode and eg-live-mode

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

;;; Code:

;; https://nullprogram.com/blog/2013/02/06/

(require 'dash)

(defmacro save-point (pred &rest body)
  "If PRED, save point before executing BODY, and restore point."
  `(progn
     (let ((old-position (when ,pred
                           (cons (line-number-at-pos)
                                 (- (point) (line-beginning-position))))))
       ,@body
       (when old-position
         (forward-line (car old-position))
         (forward-char (cdr old-position))))))

(defun eg--populate (buffer string)
  "Populate BUFFER with STRING. Preserve point if current buffer is BUFFER."
  (save-point (equal buffer (current-buffer))
    (with-current-buffer buffer
      (erase-buffer)
      (insert string)
      (goto-char (point-min)))))

(defvar eg-window-height 16)
(defun eg-switch-to-other-window-and-resize (buffer)
  "Switch to BUFFER, and set window height."
  (switch-to-buffer-other-window buffer)
  (set-window-text-height (get-buffer-window) eg-window-height)
  )

(defvar eg-master-mode-hook nil)
(defvar eg-master-mode-quit-hook nil)    ;FIXME: Use this

(defvar eg-master-toggle-key (kbd "C-c '"))
(define-minor-mode eg-master-mode
  "Get eg live on."
  :lighter " eg-live"
  :keymap
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    (define-key map eg-master-toggle-key #'eg-master)
    map))

(defvar eg-master-name "*eg-master*")
(defun eg-master ()
  "If not in 'eg-master' buffer, show 'eg-master' buffer. If in 'eg-master' buffer, sync and kill 'eg-master'."
  (interactive)
  (if (equal (current-buffer)
             (eg-master-buffer))
      (progn
        (eg-sync-master)
        (kill-buffer (eg-master-buffer))
        (delete-window))
    (progn
      (eg--populate (eg-master-buffer) (prin1-to-string (eg--local->stored eg-examples)))
      (eg-switch-to-other-window-and-resize (eg-master-buffer))
      (lispy-multiline))))

(defun eg-sync-master ()
  "Sync any changes made to 'eg-live' buffer."
  (with-current-buffer (eg-master-buffer)
    (let ((current-examples (eg--stored->local
                             (read (buffer-string)))))
      (unless (eg--local-examples-equal eg-examples current-examples)
        (setq eg-examples current-examples)
        (message "eg-examples synced")))))

(defun eg-master-buffer ()
  "Return 'eg-master' buffer, creating a new one if needed."
  (if-let (buffer (get-buffer eg-master-name))
      buffer
    (progn
      (get-buffer-create eg-master-name)
      (with-current-buffer (eg-master-buffer)
        (emacs-lisp-mode)
        (eg-master-mode)
        (current-buffer)
        ))))

(general-def
  :keymaps '(emacs-lisp-mode-map lisp-mode-map python-mode-map)
  eg-master-toggle-key 'eg-master)

(defvar eg-live-name "*eg-live*")
(defvar eg-live-toggle-key (kbd "C-c C-'"))
(define-minor-mode eg-live-mode
  "Get eg live fn on."
  :lighter " eg-live"
  :keymap
  (let ((map (make-keymap)))
    ;; (define-key map "\C-j" 'newline-and-indent)
    (define-key map
                eg-live-toggle-key
                'eg-live)
    (define-key map
                (kbd "C-c C-r")
                'eg-live-run-toggle)
    (define-key map
                (kbd "C-c C-r")
                'eg-live-run-toggle)
    map))
(defvar eg-live-fn nil)

(defun eg-live ()
  "If not in 'eg-live' buffer, show 'eg-live' buffer after prompting (or guessing) associated function. If in 'eg-live' buffer, sync and kill 'eg-live'."
  (interactive)
  (if (equal (current-buffer) (eg-live-buffer))
      (progn
        (eg-sync-live)
        (kill-buffer (eg-live-buffer))
        (delete-window))
    (progn
      (let ((saved-major-mode major-mode)
            (func
             (if (member (eg--operator) (eg--get-functions))
                 (eg--operator)
               (eg--ask-for-function "Function to Edit: "))))
        (eg-live-populate-examples func)
        (eg-switch-to-other-window-and-resize (eg-live-buffer))
        (funcall saved-major-mode))
      ))
  )

(defun eg-sync-live ()
  "Sync any changes made to 'eg-live' buffer."
  (when eg-live-fn
    (with-current-buffer (eg-live-buffer)
      (let ((fn-examples (cond ((member major-mode '(lisp-mode emacs-lisp-mode))
                                (read (buffer-string)))
                               ((eql major-mode 'python-mode)
                                (-map #'substring-no-properties
                                      (-remove (lambda (s) (or (eql ?# (string-to-char s)) (string-blank-p s))) (split-string (buffer-string) "\n")))))))
        (unless (equal (eg--get-examples eg-live-fn) fn-examples)
          (eg--set-examples eg-live-fn fn-examples)
          (message "eg-examples synced for %s" eg-live-fn))))))

(defvar eg-always-add-new-example-when-empty nil)
(defvar eg-do-not-ask-to-add-example nil)
(defun eg--print-example-strings (fn example-print-function)
  "Optimized print for performance on examples with FN using EXAMPLE-PRINT-FUNCTION. FIXME: Misnomer, not a print."
  (let* ((examples (eg--get-examples fn))
         (example-strings
          (if (null examples)
              (unless (eql major-mode 'python-mode)
                (when (and (equal (first (eg--current-list)) fn)
                           (or eg-always-add-new-example-when-empty
                               (unless eg-do-not-ask-to-add-example
                                 (yes-or-no-p "Add example under cursor?"))))
                  (list (format ";; new example\n%s" (eg--current-list)))))
            (mapcar example-print-function
                    examples))))
    (if (eql major-mode 'python-mode)
        (format "# %s\n%s"
                fn
                (string-join (-interpose "\n"
                                         examples)))
      (format ";; %s\n(%s\n )"
              fn
              (string-join (-interpose "\n "
                                       example-strings))))))

(defun eg-live-populate-examples (fn)
  "Populate 'eg-live' buffer with examples of FN."
  (setq eg-live-fn fn
        eg-showing-runs nil)
  (eg--populate
   (eg-live-buffer)
   (eg--print-example-strings fn #'prin1-to-string)))

(defun eg-live-buffer ()
  "Return 'eg-live' buffer, creating a new one if needed."
  (if-let (buffer (get-buffer eg-live-name))
      buffer
    (progn
      (get-buffer-create eg-live-name)
      (with-current-buffer (eg-live-buffer)
        (emacs-lisp-mode)
        (eg-live-mode)
        (evil-emacs-state)
        (current-buffer)
        ))))

(general-def
  :keymaps '(emacs-lisp-mode-map lisp-mode-map python-mode-map)
  eg-live-toggle-key 'eg-live)

(provide 'eg-live)

;;; eg-live.el ends here
