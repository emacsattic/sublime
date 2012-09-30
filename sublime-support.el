;;; sublime-support.el --- SublimeText emulation - Support Files
;;;
;;; Copyright (C) 2012 Lorenzo Villani.
;;;
;;; Author: Lorenzo Villani <lorenzo@villani.me>
;;; URL: https://github.com/lvillani/sublime.el
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;


;;;###autoload
(defun sublime-escape-quit ()
  "Forcefully closes the minibuffer window."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window)))
  (keyboard-escape-quit))


;;;###autoload
(defun sublime-kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))


;;;###autoload
(defun sublime-open-recent-file ()
  "Integrates `ido-completing-read' with `recentf-mode'"
  (interactive)
  (find-file (ido-completing-read "Find recent file: " recentf-list)))


;;;###autoload
(defun sublime-indent-buffer ()
  "Re-indents the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


;;;###autoload
(defun sublime-go-to-anything ()
  "Go to anything."
  (interactive)
  ()
  )

(provide 'sublime-support)

;;; sublime-support.el ends here
