;;; sublime-ui.el --- SublimeText emulation - User Interface
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


(unless (>= 24 emacs-major-version)
  (error "sublime-ui.el requires Emacs 24 or later."))


;;;###autoload
(defun sublime-set-default-font ()
  "Chooses a default font native to the platform (if available)
or fall-backs to Monospace-12."
  (interactive)
  (when (string-equal system-type "gnu/linux")
    (if (find-font (font-spec :name "Ubuntu Mono"))
        (set-default-font "Ubuntu Mono-12")
      (set-default-font "Monospace-12"))))


;;;###autoload
(defun sublime-setup-ui ()
  "Activates UI customizations."
  (interactive)
  ;; Color Theme
  (load-theme 'monokai)
  ;; Miscellaneous Settings
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-default 'truncate-lines t)
  (set-face-attribute 'show-paren-match-face nil :underline t)
  (setq cursor-type 'bar)
  (setq echo-keystrokes 0.01)
  (setq frame-title-format '("%f - " user-real-login-name "@" system-name))
  (setq inhibit-startup-screen t)
  (setq linum-format "  %d  ")
  (setq show-paren-delay 0)
  (setq truncate-partial-width-windows nil)
  ;; Minor Modes
  (blink-cursor-mode t)
  (column-number-mode t)
  (global-hl-line-mode t)
  (global-linum-mode t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode t)
  (tool-bar-mode -1)
  (which-function-mode t))


;;;###autoload
(defun sublime-ui ()
  "Activates all UI customizations defined in this module."
  (interactive)
  (sublime-set-default-font)
  (sublime-setup-ui))

(provide 'sublime-ui)

;;; sublime-ui.el ends here
