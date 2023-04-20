;;; quake.el --- Provides functions for quick access to a console buffer.

;; Copyright (C) 2018-2022 Sean Wang
;; Author: Sean Wang
;; URL: http://github.com/sww/quake.el
;; Created: 2018
;; Version: 0.3
;; Keywords: console
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides functions for quick split access to a buffer.
;; (bind-key "C-`" 'quake)
;;
;; Use a prefix argument to specify a buffer to show in the split.

;;; Code:

(defgroup quake nil
  "Provides quick split buffer access."
  :group 'tools)

(defcustom quake-window-position 'below
  "Where to create the split ('below, 'above, 'left, 'right)."
  :type '(radio ('right)
                ('below)
                ('left)
                ('above))
  :group 'quake)

(defcustom quake-max-window-size 0.3
  "The max height/width percentage of the quake buffer."
  :type 'float
  :group 'quake)

(defcustom quake-term-function 'ansi-term
  "Which term function to use."
  :type 'function
  :group 'quake)

(defcustom quake-term-args "/bin/bash"
  "Arguments to pass to quake-term-function."
  :type 'string
  :group 'quake)

(defcustom quake-buffer-name "quake"
  "The default buffer name."
  :type 'string
  :group 'quake)

(defun quake--get-buffer-name ()
  "Get or set a new quake-buffer-name."
  (when current-prefix-arg (setq quake-buffer-name (quake--select-buffer-name)))
  quake-buffer-name)

(defun quake--select-buffer-name ()
  "Select which buffer to set as the quake buffer."
  (let (buffer-name)
    (completing-read (format "Select new quake buffer (current: %s): " quake-buffer-name)
                     (mapcar (function buffer-name)
                             (buffer-list))
                     nil
                     t)))

(defun quake--set-current-buffer-as-quake-buffer ()
  "Set the current buffer as the quake buffer."
  (interactive)
  (setq quake-buffer-name (current-buffer)))

(defun quake ()
  "Provides quick access to a split buffer."
  (interactive)
  (let* ((size-func (if (member quake-window-position '(below above))
                        'window-total-height
                      'window-total-width))
         (frame-height-or-width (funcall size-func (frame-root-window)))
         (quake-buffer-name (quake--get-buffer-name))
         (quake-buffer (get-buffer quake-buffer-name))
         (quake-window (get-buffer-window quake-buffer-name))
         (quake-split-size)
         (quake-window-size))

    (cond (quake-window
           ;; Probably already ran the function, so remember the size and hide the window.
           (setq quake-window-size (funcall size-func quake-window))
           (setq quake-max-window-size (/ quake-window-size (float frame-height-or-width)))
           (delete-window quake-window))

          (t
           (setq quake-split-size (- frame-height-or-width (floor (* frame-height-or-width quake-max-window-size))))
           ;; Create the window split.
           (select-window (split-window (frame-root-window) quake-split-size quake-window-position))

           ;; Check if the buffer exists already, otherwise create a shell session.
           (if quake-buffer
               (switch-to-buffer quake-buffer)
             (funcall quake-term-function quake-term-args)
             (rename-buffer quake--get-buffer-name))))))

(provide 'quake)
;;; quake.el ends here
