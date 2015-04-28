;; M-x eval-buffer

;; Google's C/C++ Standards
;(add-to-list 'load-path "~/.emacs.d/lisp/")
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(require 'google-c-style)


;;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; todo file locations
(setq org-agenda-files '("~/.org/organizer.org" "~/.org/class.org"))
;; 12hr clock in agenda view
(setq org-agenda-timegrid-use-ampm 1)
;; agenda view display 14 days ahead (excluding current day)
(setq org-agenda-span 15)
;; default deadline warning
(setq org-deadline-warning-days 15)
;; Overwrite the current window with the agenda
;;(setq org-agenda-window-setup 'current-window)
;; prevent editing invisible text
(setq org-catch-invisible-edits 'error)
(setq org-agenda-todo-ignore-deadlines 'past) ;; not doing what i wanted it to
;; or set an integer value to restart the warning N days before deadline
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
;; Don't show any entries with a timestamp in the global todo list.
;; The idea behind this is that by setting a timestamp, you
;; have already "taken care" of this item.
(setq org-agenda-todo-ignore-timestamp 'all)
(setq org-agenda-todo-ignore-scheduled 'all)
;; line wrap
(setq org-startup-truncated nil)


;;; EMACS ;;;
;; no scrollbar/menu/etc
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)
; turn off the 3d formatting of the mode-line.
(set-face-attribute 'mode-line nil :box nil)

;; no code highlighting
; (global-font-lock-mode 0)

;; windmove and some alternative keybindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key [(meta ?B)] 'windmove-left)  ; was unbound
(global-set-key [(meta ?P)] 'windmove-up)    ; was unbound
(global-set-key [(meta ?F)] 'windmove-right) ; was unbound
(global-set-key [(meta ?N)] 'windmove-down)  ; was unbound

;; emacs packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'firecode t)
