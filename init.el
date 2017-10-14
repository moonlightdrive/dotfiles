;;; -*- lexical-binding: t -*-
;;; init.el --- the beginning

;; Copyright (C) 2017 Jyotsna <https://github.com/moonlightdrive>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; use-package.el not needed at runtime, so the following reduces load time
(eval-when-compile
  (require 'use-package))

(set-face-attribute 'default nil :height 145)
;; modeline
(line-number-mode t)
(column-number-mode t)
;; undesired startup things
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)
;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)
;; I end sentences with one space!
(setq sentence-end-double-space nil)
(show-paren-mode 1)
(setq compilation-scroll-output 'first-error)

;; windmove and some alternative keybindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key [(meta ?B)] 'windmove-left)  ; was unbound
(global-set-key [(meta ?P)] 'windmove-up)    ; was unbound
;; TODO alchemist mode rebinds these >.>
(global-set-key [(meta ?F)] 'windmove-right) ; was unbound
(global-set-key [(meta ?N)] 'windmove-down)  ; was unbound

;; for easily going back to previous window configurations
;; eg when opening org-agenda disrupts workflow
(when (fboundp 'winner-mode)
  (winner-mode 1))

(use-package whitespace
  :demand t
  :ensure nil
  :init
  (add-hook 'prog-mode-hook #'whitespace-turn-on)
  (add-hook 'text-mode-hook #'whitespace-turn-on)
  ; cleanup whitespace on save
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config (setq-default whitespace-style '(face empty tab trailing)))

(use-package leuven-theme
  :config (load-theme 'leuven t))

;;; magit ;;;;;
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :bind ("C-x M-g" . magit-dispatch-popup))
;; TODO magit + projectile?

;;; orgmode ;;;;;
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)

(setq org-directory "~/org/")
(defvar my/org-anizer (concat org-directory "anizer.org"))
(setq org-agenda-files (list my/org-anizer
                             (concat org-directory "refile.org")))
(setq org-default-notes-file (concat org-directory "refile.org"))
;; TODO do you like weekly filing?
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/anizer.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+olp+datetree "~/org/amplified.org")
         "* %?\nEntered on %U\n  %i\n  %a" :tree-type week)))


;; agenda view display 14 days ahead (excluding current day)
(setq org-agenda-span 15)
;; prevent editing invisible text
(setq org-catch-invisible-edits 'error)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
;; Don't show any entries with a timestamp in the global todo list.
;; The idea behind this is that by setting a timestamp, you
;; have already "taken care" of this item.
(setq org-agenda-todo-ignore-timestamp 'all)
(setq org-agenda-todo-ignore-scheduled 'all)
;; repeating events only show up on the first upcoming date they are scheduled
(setq org-agenda-repeating-timestamp-show-all nil)
;; line wrap
(setq org-startup-truncated nil)
;; refiling
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

;;; HASKELL ;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; ocaml ;;;;;
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (load (expand-file-name "emacs/site-lisp/tuareg-site-file" opam-share))
    (require 'ocp-indent)
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)))

;; registers
(set-register ?o (cons 'file my/org-anizer))
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?w '(file . "~/org/amplified.org"))

;;; elixir
(use-package elixir-mode
  :commands elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package alchemist
  :commands alchemist-mode
  :diminish alchemist
  :init
  (setq alchemist-key-command-prefix (kbd "C-c k"))
  :config
  (bind-keys :map alchemist-mode-map
             ("C-c C-l" . (lambda () (interactive)
			    (save-buffer)
                            (alchemist-iex-compile-this-buffer))))
  (bind-keys :map alchemist-mode-map
             ("C-x C-e" . alchemist-iex-send-current-line))
  :mode (("\\.eex\\'" . alchemist-mode)))

;;; web-mode
(use-package web-mode
  :config
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-style 2)
  :mode ("\\.eex\\'" . web-mode))
;; add .emacs.d/modules to load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))
;; (add-to-list 'load-path (concat dotfiles-dir "modules"))

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)
