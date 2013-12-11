;;
;; window appearrence
;;

;; http://d.hatena.ne.jp/kei10in/20101101/1288617632

(defvar current-background-type 'light)

(defun toggle-current-background-type ()
  (interactive)
  (setq current-background-type
        (if (eq current-background-type 'light)
            'dark
          'light))
  (set-current-background-type current-background-type))

(defun set-current-background-type (background-type)
  (let (fg bg alpha)
    (if (eq background-type 'dark)
        (setq fg "lavender"
              bg "black"
              alpha '(85 70))
      (setq fg "black"
            bg "lavender"
            alpha '(100 100)))
    (set-foreground-color fg)
    (set-background-color bg)
    (set-frame-parameter (selected-frame) 'alpha alpha)
    (add-to-list 'default-frame-alist (cons 'background-color bg))
    ))

(when window-system
  (set-current-background-type current-background-type))

(custom-set-faces
 '(cursor
   (
    (((class color) (background light))
     (:background "black"))
    (((class color) (background dark))
     (:background "white")))))

(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "white"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;; (setq hl-line-face 'underline)
(global-hl-line-mode)

(setq visible-bell t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'set-specifier)   (set-specifier scrollbar-width 0))
(if (fboundp 'show-paren-mode) (show-paren-mode))
(setq frame-title-format (format "%%f - Emacs@%s" hostname))

(setq next-line-add-newlines t)

(custom-set-variables
 '(display-time-mode t)
 '(tool-bar-mode nil)
 )


;;;
;;; Save and Load frame configuration
;;;

(defvar frame-stored-filename "~/.emacs.d/frame-config")

(defun save-frame-configuration ()
  (interactive)
  (with-temp-file frame-stored-filename
    (prin1
     `(modify-frame-parameters
       (selected-frame)
       (quote
  ,(mapcar (lambda (p)
       (assoc p (frame-parameters (selected-frame))))
     '(width height left top))))
     (current-buffer))))

(defun load-frame-configuration ()
  (interactive)
  (and (file-exists-p frame-stored-filename)
       (load-file frame-stored-filename)))

;;;
;;; frame movement
;;;

(setq windmove-wrap-around t)

(let ((modifier 'meta))
  (global-set-key (vector (list modifier ?h)) 'windmove-left)
  (global-set-key (vector (list modifier ?l)) 'windmove-right)
  (global-set-key (vector (list modifier ?k)) 'windmove-up)
  (global-set-key (vector (list modifier ?j)) 'windmove-down))

(global-set-key "\C-t" 'other-window)
