;;; sublime-system.el --- SublimeText emulation - System
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
(defun sublime-setup-clipboard ()
  "Improve interaction with X11 clipboard giving Emacs the 'feel'
of a modern X11 application."
  (interactive)
  (setq mouse-drag-copy-region nil)
  (setq select-active-regions t)
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary nil)
  (global-set-key "\C-w" 'clipboard-kill-region)
  (global-set-key "\C-y" 'clipboard-yank)
  (global-set-key "\M-w" 'clipboard-kill-ring-save)
  (global-set-key [mouse-2] 'mouse-yank-primary))

;;;###autoload
(defun sublime-setup-electric-minor-modes ()
  "Enables automatic matching of parentheses and newline
insertion around some characters."
  (electric-layout-mode)
  (electric-pair-mode))

;;;###autoload
(defun sublime-setup-elpa-repositories ()
  "Configures ELPA to use the GNU and Marmalade repositories."
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;;;###autoload
(defun sublime-setup-file-hooks ()
  (interactive)
  ;; Flyspell
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ;; Before/After Save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook 'time-stamp)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p nil t))

;;;###autoload
(defun sublime-setup-misc-editing-options ()
  "Miscellaneous editing properties."
  (interactive)
  (setq auto-save-default nil)
  (setq backup-inhibited t)
  (setq fill-column 74)
  (setq indent-tabs-mode nil)
  (setq indicate-empty-lines t)
  (setq require-final-newline t)
  (setq tab-width 4))

;;;###autoload
(defun sublime-setup-mode-assoc ()
  "Additional associations between file types and major modes."
  (interactive)
  (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . text-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

;;;###autoload
(defun sublime-setup-recentf ()
  "Configures `recentf' for use in combination with `ido-mode'"
  (setq recentf-max-saved-items 75)
  (global-set-key (kbd "C-x C-r") 'sublime-open-recent-file)
  (recentf-mode t))

;;;###autoload
(defun sublime-setup-snippets ()
  "Enables TextMate-style snippets."
  (interactive)
  (require 'yasnippet)
  (yas/load-directory "~/.emacs.d/snippets")
  (yas/global-mode))

;;;###autoload
(defun sublime-system ()
  "Enables all miscellaneous customizations."
  (interactive)
  (sublime-setup-clipboard)
  (sublime-setup-electric-minor-modes)
  (sublime-setup-elpa-repositories)
  (sublime-setup-file-hooks)
  (sublime-setup-misc-editing-options)
  (sublime-setup-mode-assoc)
  (sublime-setup-recentf)
  (sublime-setup-snippets))

(provide 'sublime-system)

;;; sublime-system.el ends here
