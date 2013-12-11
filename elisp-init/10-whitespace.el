;;;
;;; White space check
;;;

(require 'whitespace)

(setq-default
 tab-width        8
 indent-tabs-mode nil
 ruby-indent-tabs-mode nil)

(setq whitespace-action '(report-on-bogus))

;; Double-width whitespace (　) TAB(	)
;; http://cloverrose.hateblo.jp/entry/2013/04/12/041758
(setq whitespace-style '(face trailing tabs spaces empty space-mark))

(setq whitespace-display-mappings
      '(
        ;; (space-mark ?\ [?\u00B7] [?.]) ; space - centered dot
        (space-mark ?\xA0 [?\u00A4] [?_]) ; hard space - currency
        (space-mark ?\x8A0 [?\x8A4] [?_]) ; hard space - currency
        (space-mark ?\x920 [?\x924] [?_]) ; hard space - currency
        (space-mark ?\xE20 [?\xE24] [?_]) ; hard space - currency
        (space-mark ?\xF20 [?\xF24] [?_]) ; hard space - currency
        (space-mark ?\u3000 [?\u25a1] [?_ ?_]) ; full-width-space - square
        ;; NEWLINE is displayed using the face `whitespace-newline'
        ;; (newline-mark ?\n [?$ ?\n]) ; eol - dollar sign
        ;; (newline-mark ?\n [?\u21B5 ?\n] [?$ ?\n]) ; eol - downwards arrow
        ;; (newline-mark ?\n [?\u00B6 ?\n] [?$ ?\n]) ; eol - pilcrow
        ;; (newline-mark ?\n [?\x8AF ?\n] [?$ ?\n]) ; eol - overscore
        ;; (newline-mark ?\n [?\x8AC ?\n] [?$ ?\n]) ; eol - negation
        ;; (newline-mark ?\n [?\x8B0 ?\n] [?$ ?\n]) ; eol - grade
        ;;
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        ;; (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]) ; tab - left quote mark
        ))

;; Highlight Double-width whitespace (　)
(setq whitespace-space-regexp "\\(\u3000+\\)")
(set-face-foreground 'whitespace-space 'nil)
(set-face-background 'whitespace-space "cyan")

(setq whitespace-global-modes
      (let ((valid-list ()) (mode-sym))
        (mapcar
         (lambda (mode)
           (setq mode-sym (cdr mode))
           (if (and mode-sym (symbolp mode-sym)
                    (not (memq mode-sym valid-list)))
               (setq valid-list (cons mode-sym valid-list))))
         auto-mode-alist)
        valid-list))

(setq whitespace-global-modes
      (append '(makefile-mode mew-draft-mode mhc-draft-mode)
              whitespace-global-modes))

(global-whitespace-mode t)

(add-hook 'whitespace-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; Makefile for tab size 8
;;
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 8)))
