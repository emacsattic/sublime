;;; sublime-project.el --- SublimeText emulation - Project Management
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

(defun sublime-go-to-anything ()
  "Starts gpicker from the root directory of the current EDE
project (if we are inside one). Otherwise, shows a picker dialog
using the current buffer's directory as root."
  (interactive)
  (if (not (eq nil ede-object-root-project))
      ;; Inside an EDE project:
      (progn
        (gpicker-visit-project (ede-project-root))
        (gpicker-find-file))
    ;; Outside a project:
    (progn
      (gpicker-visit-project (file-name-directory (buffer-file-name)))
      (gpicker-find-file))))

;;;###autoload
(defun sublime-setup-project-management ()
  "Standard EDE configuration."
  (interactive)
  (setq stack-trace-on-error nil)
  (global-ede-mode t))

;;;###autoload
(defun sublime-setup-go-to-anything ()
  "Emulates SublimeText `Goto Anything' feature using gpicker."
  (interactive)
  (let ((gpicker-el "/usr/share/doc/gpicker/gpicker.el.gz"))
    (when (file-exists-p gpicker-el)
      (load gpicker-el)
      (global-set-key (kbd "C-p") 'sublime-go-to-anything))))

;;;###autoload
(defun sublime-project ()
  "Activates all project management customizations."
  (interactive)
  (sublime-setup-project-management)
  (sublime-setup-go-to-anything))

(provide 'sublime-project)

;;; sublime-project.el ends here
