;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;;; EMACS ;;;;;
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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

;; windmove and some alternative keybindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key [(meta ?B)] 'windmove-left)  ; was unbound
(global-set-key [(meta ?P)] 'windmove-up)    ; was unbound
(global-set-key [(meta ?F)] 'windmove-right) ; was unbound
(global-set-key [(meta ?N)] 'windmove-down)  ; was unbound

;; for easily going back to previous window configurations
;; eg when opening org-agenda disrupts workflow
(when (fboundp 'winner-mode)
  (winner-mode 1))

(load-theme 'monokai t)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;; HASKELL ;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
