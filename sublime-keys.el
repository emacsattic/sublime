;;; sublime-keys.el --- SublimeText emulation - Keyboard Shortcuts
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
(defun sublime-setup-fuzzy-command-prompt ()
  "Fuzzy M-x command prompt thanks to `IDO' and `SMEX'."
  (interactive)
  ;; Configuration Variables
  (setq ido-create-new-buffer 'always)
  (setq ido-everywhere t)
  (setq ido-ignore-extensions t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-auto-merge-work-directories-length nil)
  (setq ido-enable-prefix nil)
  (setq ido-max-prospects 8)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-enable-flex-matching t)
  ;; Commands
  (ido-mode t)
  (ido-ubiquitous t)
  (smex-initialize)
  (global-set-key (kbd "C-S-p") 'smex))

;;;###autoload
(defun sublime-setup-keys ()
  "Additional keybindings Setup additional CUA keybindings."
  (interactive)
  ;; CUA Mode
  (cua-mode t)
  ;; Editing
  (global-set-key (kbd "C-/") 'comment-or-uncomment-region)
  (global-set-key (kbd "C-<backspace>") 'backward-kill-word)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  (global-set-key (kbd "RET") 'newline-and-indent)
  ;; Buffer Navigation
  (global-set-key (kbd "C-<next>") 'next-buffer)
  (global-set-key (kbd "C-<prior>") 'previous-buffer)
  (global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "<f3>") 'isearch-forward)
  ;; Custom Functions
  (global-set-key (kbd "<escape>") 'sublime-escape-quit)
  (global-set-key (kbd "<f12>") 'sublime-indent-buffer)
  (global-set-key (kbd "C-o") 'ido-find-file)
  (global-set-key (kbd "C-w") 'sublime-kill-current-buffer))

;;;###autoload
(defun sublime-keys ()
  "Activates all keyboard customizations."
  (interactive)
  (sublime-set-default-font)
  (sublime-setup-fuzzy-command-prompt)
  (sublime-setup-keys)
  (sublime-setup-ui))

(provide 'sublime-keys)

;;; sublime-keys.el ends here
