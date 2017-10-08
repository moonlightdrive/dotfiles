;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;;; EMACS ;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(set-face-attribute 'default nil :height 120)
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
(global-set-key [(meta ?F)] 'windmove-right) ; was unbound
(global-set-key [(meta ?N)] 'windmove-down)  ; was unbound

;; for easily going back to previous window configurations
;; eg when opening org-agenda disrupts workflow
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; took this from someone but don't recall who :c
(use-package whitespace
  :demand t
  :ensure nil
  :init
  (add-hook 'prog-mode-hook #'whitespace-turn-on)
  (add-hook 'text-mode-hook #'whitespace-turn-on)
  ; cleanup whitespace on save
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config (setq-default whitespace-style '(face empty tab trailing)))

(load-theme 'monokai t)

;; magit
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :bind ("C-x M-g" . magit-dispatch-popup))

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
;; (require 'ocp-indent)
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
  :mode (("\\.eex\\'" alchemist-mode)))

;;; web-mode
(use-package web-mode
  :config
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-style 2))
