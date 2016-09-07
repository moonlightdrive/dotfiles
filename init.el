;; M-x eval-buffer

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(add-to-list 'load-path "~/.emacs.d/lisp/")


;;; ocaml ;;;;;
;; https://github.com/realworldocaml/book/wiki/Installation-Instructions
;; paths
(setq opam-share (substring
                  (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
;;; TODO see https://opam.ocaml.org/doc/Tricks.html

;; Load merlin-mode
(require 'merlin)
; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(global-set-key (kbd "C-c t") 'merlin-type-expr)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

;; indentation
(require 'ocp-indent)


(add-hook 'tuareg-mode-hook (lambda ()
                              (setq indent-line-function 'ocp-indent-line)))
; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(setq tuareg-font-lock-symbols t)
;; utop
;; Automatically load utop.el
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

;; make OCaml-generated files invisible to filename completion
;; http://chaudhuri.info/misc/osetup/
;;;(mapc #'(lambda (ext) (add-to-list 'completion-ignored-extensions ext))
;;;  '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".annot"))


;;; golang ;;;;;
(add-hook 'before-save-hook 'gofmt-before-save)


;;; TRAMP ;;;;;
(require 'tramp)
(setq tramp-default-method "scp")

;;; EMACS ;;;;;
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(load "server")
(unless (server-running-p) (server-start))

;; registers
(set-register ?o (cons 'file "~/.org/organizer.org"))

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/moe-theme.el/")
;;(load-theme 'monokai t)
;;(load-theme 'moe-light t)
(require 'moe-theme)
;; Resize titles (optional).
;;(setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
;;(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
;;(setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
(moe-theme-set-color 'yellow)
(setq moe-theme-highlight-buffer-id nil)
;; dark/light depending on time of day see Readme
;; (require 'moe-theme-switcher)
(show-paren-mode t)
(setq show-paren-style 'expression)
(moe-dark)

;; font
(set-frame-font "Droid Sans Mono 8")

;; modeline
(line-number-mode t)
(column-number-mode t)
;; undesired startup things
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)
;; set fringe to be same as bg
(set-face-attribute 'fringe nil
		    :background (face-background 'default)
		    :foreground (face-foreground 'default))
;; no code colorizing
;; TODO but I want to color comments!
;(global-font-lock-mode 0)
(add-hook 'emacs-lisp-mode-hook 'font-lock-mode)
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


;;; haskell ;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; haskell interactive mode
;; ;; M-x eval-buffer

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(add-to-list 'load-path "~/.emacs.d/lisp/")


;;; ocaml ;;;;;
;; https://github.com/realworldocaml/book/wiki/Installation-Instructions
;; paths
(setq opam-share (substring
                  (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;;; TODO see https://opam.ocaml.org/doc/Tricks.html

;; Load merlin-mode
(require 'merlin)
; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(global-set-key (kbd "C-c t") 'merlin-type-expr)
;(setq merlin-use-auto-complete-mode t)
;(setq merlin-error-after-save nil)

;; Enable auto-complete
;;;(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
;;;(setq merlin-command 'opam)
;; indentation
(require 'ocp-indent)
;; (eval "ocp-indent.el")

(add-hook 'tuareg-mode-hook (lambda ()
                              (setq indent-line-function 'ocp-indent-line)))
; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(setq tuareg-font-lock-symbols t)
;; utop
;; Automatically load utop.el
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

;; make OCaml-generated files invisible to filename completion
;; http://chaudhuri.info/misc/osetup/
;;;(mapc #'(lambda (ext) (add-to-list 'completion-ignored-extensions ext))
;;;  '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".annot"))


;;; TRAMP ;;;;;
(require 'tramp)
(setq tramp-default-method "scp")

;;; EMACS ;;;;;
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(load "server")
(unless (server-running-p) (server-start))

;; registers
(set-register ?o (cons 'file "~/.org/organizer.org"))

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/moe-theme.el/")
;;(load-theme 'monokai t)
;;(load-theme 'moe-light t)
(require 'moe-theme)
;; Resize titles (optional).
;;(setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
;;(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
;;(setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
(moe-theme-set-color 'yellow)
(setq moe-theme-highlight-buffer-id nil)
;; dark/light depending on time of day see Readme
;; (require 'moe-theme-switcher)
(show-paren-mode t)
(setq show-paren-style 'expression)
;; (moe-light)

;; font
(set-frame-font "Droid Sans Mono 8")

;; modeline
(line-number-mode t)
(column-number-mode t)
;; undesired startup things
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; change "yes or no" to "y or n"
(fset 'yes-or-no-p 'y-or-n-p)
;; set fringe to be same as bg
(set-face-attribute 'fringe nil
		    :background (face-background 'default)
		    :foreground (face-foreground 'default))
;; no code colorizing
;; TODO but I want to color comments!
;(global-font-lock-mode 0)
(add-hook 'emacs-lisp-mode-hook 'font-lock-mode)
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


;;; haskell ;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; haskell interactive mode
;; https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-Setup
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;;; latex
;; font-lock for cuties
;; http://endlessparentheses.com/improving-latex-equations-with-font-lock.html
(defface endless/unimportant-latex-face
  '((t :height 0.7
       :inherit font-lock-comment-face))
  "Face used on less relevant math commands.")

(font-lock-add-keywords
 'latex-mode
 `((,(rx (or (and "\\" (or (any ",.!;")
                           (and (or "left" "right"
                                    "big" "Big")
                                symbol-end)))
             (any "_^")))
    0 'endless/unimportant-latex-face prepend))
 'end)


;;; org mode ;;;;;
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)
;; file locations
(setq org-directory '"~/.org/")
(setq org-agenda-files '("~/.org/organizer.org" "~/.org/class.org"))
(setq org-default-notes-file (concat org-directory "refile.org"))
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
(setq org-agenda-todo-ignore-deadlines 'past) ;; not doing what i wanted 
;; or set an integer value to restart the warning N days before deadline
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
;; Don't show any entries with a timestamp in the global todo list.
;; The idea behind this is that by setting a timestamp, you
;; have already "taken care" of this item.
(setq org-agenda-todo-ignore-timestamp 'all)
(setq org-agenda-todo-ignore-scheduled 'all)
;; line wrap
(setq org-startup-truncated nil)
(setq org-todo-keywords
 '((sequence
    "TODO"  ; next action
    "|" "DONE(x)" "CANCELLED(c@)")
   ))
; refiling
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
