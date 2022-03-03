;;; quake.el --- Provides functions for quick access to a console buffer.

;; Copyright (C) 2018 Sean Wang
;; Author: Sean Wang
;; URL: http://github.com/sww/quake.el
;; Created: 2018
;; Version: 0.2
;; Keywords: console
;; Package-Requires:

;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides functions for quick access to a console buffer.
;; (bind-key "C-`" 'quake)
;;
;; Use a prefix argument to specify a buffer to show in the split.

;;; Code:

(defgroup quake nil
  "Provides quick console access."
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
  (let ((buffer-name "")
        (current quake-buffer-name))
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
  "Provides Quake-like buffer splits."
  (interactive)
  (let (buffer-name height-or-width quake-split-size)
    (setq buffer-name (quake--get-buffer-name))
    (setq height-or-width (if (member quake-window-position '(below above))
                              (frame-height)
                            (frame-width)))
    (setq quake-split-size (- height-or-width (floor (* height-or-width quake-max-window-size))))

    (if (get-buffer-window buffer-name)
        ;; Probably already ran the function, so hide the frame.
        (delete-window (get-buffer-window buffer-name))

      ;; Create the window split.
      (select-window (split-window (frame-root-window) quake-split-size))

      ;; Check if a shell buffer exists already, or create one.
      (if (get-buffer buffer-name)
          (switch-to-buffer (get-buffer buffer-name))
        (funcall quake-term-function quake-term-args)
        (rename-buffer buffer-name)))))

(provide 'quake)
;;; quake.el ends here
