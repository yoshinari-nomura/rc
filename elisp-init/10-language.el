;;;
;;; Language Environment
;;;

(set-language-environment "Japanese")

(let* ((LANG (or (getenv "LANG") "ja_JP.UTF-8"))
       (case-fold-search t)
       (my-prefer-coding-system
        (cond
         ((string-match "utf"  LANG)  'utf-8-unix)
         ((string-match "euc"  LANG)  'euc-jp-unix)
         ((string-match "sjis" LANG)  'sjis-unix)
         (t                           'iso-2022-jp-unix)
         )))
  (set-default-coding-systems my-prefer-coding-system)
  (prefer-coding-system my-prefer-coding-system))


;;
;; http://email.esm.psu.edu/pipermail/macosx-emacs/2011-August/002767.html
;;
;; Convert UTF NFD to NFC in region.
;;   Pasting characters from Preview.app decomposes UTF characters.
;;   Utfix ties up the decomposed characters.
;;
(defun utfix (rs re)
  (interactive "r")
  (save-excursion
    (goto-char rs)
    (cond
     ;; Emacs 23
     ((equal emacs-major-version 23)
      (utf-8m-post-read-latin-conversion (- re rs)))
     ((equal emacs-major-version 24)
      (ns-utf8-nfd-post-read-conversion (- re rs)))
     )
    ))
