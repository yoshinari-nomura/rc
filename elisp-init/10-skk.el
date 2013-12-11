;;
;; -*-Emacs-Lisp-*-
;;

(require 'skk-setup)

;;
;; Helper functions
;;

(defun which-file (filename-list)
  (catch 'found
    (while filename-list
      (if (file-exists-p (expand-file-name (car filename-list)))
          (throw 'found (car filename-list)))
      (setq filename-list (cdr filename-list)))))

;;
;; load-path
;;

(setq load-path
      (append (list (expand-file-name "~/sys/lib/elisp/skk")
                    "/usr/local/share/emacs/site-listp/skk")
              load-path))
;;
;; auto-load
;;

(autoload 'skk-mode "skk" nil t)
(autoload 'skk-auto-fill-mode "skk" nil t)
(autoload 'skk-tutorial "skk-tut" nil t)
(autoload 'skk-check-jisyo "skk-tools" nil t)
(autoload 'skk-merge "skk-tools" nil t)
(autoload 'skk-diff "skk-tools" nil t)
(autoload 'skk-isearch-mode-setup "skk-isearch" nil t)
(autoload 'skk-isearch-mode-cleanup "skk-isearch" nil t)

;;
;; skksvr and JISYO path
;;

(setq

 skk-server-host "localhost"

 skk-server-prog
 (which-file
  '(
    "/home/QtPalmtop/skkserv/skkserv"  ;; for Zaurus
    "/usr/sbin/skkserv.rb"             ;; for Vine 2.6
    "/usr/local/libexec/skkserv"       ;; for Solaris (ace)
    "/usr/local/sbin/skkserv"          ;; for FreeBSD
    ))

 skk-server-jisyo
 (which-file
  '(
    "/home/QtPalmtop/share/SKK-JISYO"
    "~/sys/lib/elisp/skk/data/SKK-JISYO.L"
    ))

 skk-aux-large-jisyo
 (which-file
  '(
    "~/sys/lib/elisp/skk/dic/SKK-JISYO.L"
    "~/sys/lib/elisp/skk/data/SKK-JISYO.L"
    "/usr/share/emacs/site-listp/skk/jisyo/SKK-JISYO.L"
    )))

;;
;; Always invoke skk isearch.
;;

(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

;;
;; Change some behaviors.
;;

(setq
 skk-egg-like-newline          t
 skk-auto-okuri-process        nil
 skk-isearch-start-mode        'ascii
 skk-isearch-use-previous-mode nil
 skk-delete-implies-kakutei    nil
 skk-rom-kana-rule-list
 '(
   ("nn" nil ("ン" . "ん"))
   ("n'" nil ("ン" . "ん"))
   ("n:" nil ("ン" . "ん"))
   ("z " nil "　")
   ("?"  nil "?")
   (":"  nil ":")
   ("@"  nil "@")
   (","  nil "，")
   ("."  nil "．")
   ("\\" nil "\\")
   ))
