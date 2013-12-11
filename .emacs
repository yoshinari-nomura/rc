;;; .emacs --- Emacs startup file.

;; Author:  Yoshinari Nomura
;; Created: 2010-01-14 13:59:02 JST
;; Revised Time-stamp: <2013-12-10 14:41:07 JST>

;; (setq debug-on-error t)

;;;
;;; Package repository settings
;;;

(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless (require 'melpa nil t)
  (switch-to-buffer
   (url-retrieve-synchronously
    "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
  (package-install-from-buffer  (package-buffer-info) 'single)
  (require 'melpa))

;;;
;;; Load individual init files
;;;

;; Add all subdirectories of `default-directory' to `load-path'.
(let ((default-directory "~/sys/lib/elisp"))
  (normal-top-level-add-subdirs-to-load-path))

;; Load all files in `private-init-directory'.
(setq private-init-directory "~/sys/lib/rc/elisp-init/")
(mapc 'load (directory-files private-init-directory t "^[0-9].*.elc?$"))

;;;
;;; Load Custom file
;;;   custom-file: file used for storing customization information.
;;;

(setq custom-file (expand-file-name
                   "XX-emacs-custom.el" private-init-directory))
(load custom-file)

;;;
;;; Enable commands
;;;

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;;
;;; Start emacs server
;;;

(require 'server)

(unless (server-running-p)
  (server-start))

;;;
;;; Restore frame configuration
;;;

(load-frame-configuration)

;;;
;;; Done
;;;

(message "Done.")

;;; Local Variables:
;;; mode: emacs-lisp
;;; time-stamp-active: t
;;; End:
