;;;
;;; global keyboard and mouse bindings.
;;;

;; Control-h is BS
(load-library "term/bobcat")
(setq normal-erase-is-backspace nil) ;; for emacs 22 above
(terminal-init-bobcat)

;; for minibuffer
(define-key minibuffer-local-filename-completion-map
  " " 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-filename-map
  " " 'minibuffer-complete-word)

;; swap command-Key and option-key for Mac
(when (and (eq system-type 'darwin) (>= emacs-major-version 23))
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super)))

;; replace primary keys
(global-set-key "\C-xi"    'insert-file-or-filename)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj"    'skk-auto-fill-mode)
(global-set-key "\C-xt"    'skk-tutorial)

(if (or (eq window-system 'mac) (eq window-system 'ns))
    (global-set-key "\M-m" 'iconify-or-deiconify-frame))

(global-set-key "\M-n" 'mac-preview-next-page)
(global-set-key "\M-p" 'mac-preview-previous-page)

;; Control-C

(global-set-key "\C-c.."   'mhc-goto-this-month-with-push)
(global-set-key "\C-c.n"   'mhc-goto-next-month-with-push)
(global-set-key "\C-c.p"   'mhc-goto-prev-month-with-push)

;; Control-O
(global-unset-key "\C-o")

(global-set-key "\C-o\C-o" 'open-calendar)
(global-set-key "\C-ob" 'git-blame-at-point)
(global-set-key "\C-oc" 'org-capture)
(global-set-key "\C-od" 'dic-at-point)
(global-set-key "\C-or" 'org-remember)
(global-set-key "\C-oa" 'org-agenda)
(global-set-key "\C-ol" 'org-store-link)
(global-set-key "\C-ob" 'org-iswitchb)
(global-set-key "\C-ox" 'anything)
(global-set-key "\C-og" 'org-mac-grab-link)

;; Control-Z
(global-unset-key "\C-z")

(global-set-key "\C-zh" 'help)
(global-set-key "\C-z\C-i" 'trueup-columns-region)
(global-set-key "\C-z\C-l" 'msp-scan-buffer)
(global-set-key "\C-z\C-m" 'msp-browse-url-at-point)
(global-set-key "\C-z\C-o"
                (lambda ()
                  (interactive)
                  (browse-url
                   (concat "file://" (expand-file-name default-directory)))))
(global-set-key "\C-z\C-z" 'suspend-emacs)

(global-set-key "\C-za"    'address)
(global-set-key "\C-zd"    '(lambda () (interactive) (mew-summary-visit-folder "+Drafts")))
(global-set-key "\C-zi"    'mew-push-and-inc)
(global-set-key "\C-zm"    'memo)
(global-set-key "\C-zt"    'todo)
(global-set-key "\C-zo"    'org-clock-goto)

;; Mouse and function keys
(global-set-key [mouse-2]           'msp-browse-url-at-point)
(global-set-key [f2]                'ecb-toggle)
(global-set-key [(control shift l)] 'other-frame)
(global-set-key [(control shift f)] 'maxframe-toggle-fullscreen)

;;
;; Remap font size
;;
(global-set-key [(super ?+)] (lambda () (interactive) (text-scale-increase  1)))
(global-set-key [(super ?-)] (lambda () (interactive) (text-scale-increase -1)))
(global-set-key [(super ?0)] (lambda () (interactive) (text-scale-increase  0)))
(global-set-key [(super ?8)] (lambda () (interactive) (toggle-current-background-type)))
