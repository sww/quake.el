;;; quake.el --- Provides functions for quick access to a console buffer.

;; Copyright (C) 2018 Sean Wang
;; Author: Sean Wang
;; URL: http://github.com/sww/quake.el
;; Created: 2018
;; Version: 0.1
;; Keywords: console
;; Package-Requires:

;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides functions for quick access to a console buffer.
;; (bind-key "C-`" 'quake)

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
  "The max height/width of the quake buffer."
  :type 'float
  :group 'quake)

(defcustom quake-buffer-name "quake"
  "The buffer name."
  :type 'string
  :group 'quake)

(defun quake ()
  "Provides Quake-like buffer splits."
  (interactive)
  (defvar running-buffer-name)

  (setq running-buffer-name (concat "*"
                                    quake-buffer-name
                                    "*"))

  (if (get-buffer-window running-buffer-name)
      ;; Probably already ran the function, so hide the frame.
      (delete-window (get-buffer-window running-buffer-name))

    (defvar height-or-width
      (if (member quake-window-position '("below" "above"))
          (frame-height)
        (frame-width)))

    ;; Create the window split and focus to the new window.
    (select-window
     (split-window
      (frame-root-window)
      (floor (- height-or-width (* height-or-width
                                   quake-max-window-size)))
      quake-window-position))

    ;; Check if a shell buffer exists already, or create one.
    (if (get-buffer running-buffer-name)
        (switch-to-buffer (get-buffer running-buffer-name))
      (ansi-term (getenv "SHELL") quake-buffer-name))))

(provide 'quake)
;;; quake.el ends here
