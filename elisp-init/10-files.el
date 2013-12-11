;;;
;;; recentf
;;;

(require 'recentf)

(defvar my-recentf-list-prev nil)

(defadvice recentf-save-list
  (around no-message activate)
  "If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
  (unless (equal recentf-list my-recentf-list-prev)
    (flet ((message (format-string &rest args)
                    (eval `(format ,format-string ,@args)))
           (write-file (file &optional confirm)
                       (let ((str (buffer-string)))
                         (with-temp-file file
                           (insert str)))))
      ad-do-it
      (setq my-recentf-list-prev recentf-list))))

(defadvice recentf-cleanup
  (around no-message activate)
  "suppress the output from `message' to minibuffer"
  (flet ((message (format-string &rest args)
                  (eval `(format ,format-string ,@args))))
    ad-do-it))

(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-max-saved-items 2000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(run-with-idle-timer 30 t 'recentf-save-list)
(recentf-mode 1)

;;;
;;; backup files
;;; directory is automatically created.
;;;
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))

(setq
  version-control t     ;; add version number to backup files like: filename.el.~100~
  delete-old-versions t ;;
  kept-new-versions 100 ;; save newest
  kept-old-versions 2   ;; keep some oldests, you may need the initial backup which made at the first save.
  )

;;;
;;; auto save
;;; directory is automatically created.
;;;
(setq auto-save-file-name-transforms
      `((".*"  ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

(setq auto-save-list-file-prefix
      (expand-file-name "save-list/" user-emacs-directory))

;;;
;;; Info
;;;
(setq Info-default-directory-list
      (append (list (expand-file-name "~/sys/lib/info"))
              Info-default-directory-list))

;;;
;;; Bookmark
;;;
(setq bookmark-default-file
      (expand-file-name "emacs.bmk" user-emacs-directory))

;;;
;;; UTF NFD (Normalization Form by Decomposition) for Mac OSX HFS+
;;; http://www.sakito.com/2010/05/mac-os-x-normalization.html
;;;
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (setq process-coding-system-alist
        '(("^ls$"     . utf-8-hfs)
          ("^locate$" . utf-8-hfs)
          ("project-list" . utf-8-hfs))))
