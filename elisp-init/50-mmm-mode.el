;;
;; mmm mode settings
;; cf. http://lists.gnu.org/archive/html/emacs-orgmode/2010-03/msg00043.html
;;     http://wiki.livedoor.jp/kou1okada/d/emacs%20-%20mmm-mode
;;

;;(if (fboundp 'add-private-load-path)
;;    (add-private-load-path "mmm-mode"))

(setq mmm-set-file-name-for-modes '(mew-draft-mode mhc-draft-mode))

(require 'mmm-mode) ;; or (require 'mmm-auto)

(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)

(defun buffer-local-variables-in-mode (mode)
  (with-temp-buffer
    (funcall mode)
    (buffer-local-variables)))

(defun buffer-local-variable-symbols-in-mode (mode)
  (let* ((base-assoc (buffer-local-variables-in-mode 'fundamental-mode))
         (mode-assoc (buffer-local-variables-in-mode mode))
         (mode-vars  (mapcar 'car mode-assoc)))
    (delq nil (mapcar
               (lambda (var)
                 (unless (or (memq var '(mode-name major-mode))
                             (equal (assoc var base-assoc)
                                    (assoc var mode-assoc)))
                   var))
               mode-vars))))

(defun mmm-save-local-variables-in-mode (mode)
  (dolist (v (buffer-local-variable-symbols-in-mode mode))
    (add-to-list 'mmm-save-local-variables `(,v buffer (,mode)))))

(mmm-save-local-variables-in-mode 'org-mode)

(defadvice recenter (after mmm-parse-buffer-after-recenter activate compile)
  (if (and (fboundp 'mmm-parse-buffer)
           (fboundp 'mmm-get-all-classes)
           (mmm-get-all-classes nil))
      (save-excursion
        (mmm-parse-buffer))))

;; C-cC-c on an org-table invokes ``org-table-recalculate-buffer-tables'' or
;; ``org-table-iterate-buffer-tables''.
;; These functions search all org-tables in the current buffer.
;; If the buffer has some text with read-only property,
;; these functions seem to be stalled on that.
;;
;; This problem is exposed when
;; a mew-draft-mode buffer encloses an org-table with the help of mmm-mode,
;; because the mew-draft has some text with read-only
;; property like a ``----''.
;;
;; To avoid this problem, ``org-table-recalculate'' should be called with
;; inhibit-read-only is t.
;;
(defadvice org-table-recalculate
  (around org-table-recalculate-advice)
  "org-tagle-recalculate with read-only text property"
  (let ((inhibit-read-only t))
    ad-do-it))

(ad-activate 'org-table-recalculate)

(defadvice mmm-ify-by-class (after refontify-after-mmm activate compile)
  (font-lock-fontify-buffer))

(mmm-add-mode-ext-class 'mew-draft-mode  nil  'org-in-draft)
(mmm-add-mode-ext-class 'mhc-draft-mode  nil  'org-in-draft)
(mmm-add-mode-ext-class 'text-mode       nil  'org-in-draft)
(mmm-add-mode-ext-class 'ruby-mode       nil  'org-in-here-document)
(mmm-add-mode-ext-class 'emacs-lisp-mode nil  'org-in-here-document)
(mmm-add-mode-ext-class nil       "^[0-9]+$"  'org-in-draft)

;; Org-mode starts with * on the beginning-of-line
;; ends with non-header character on the beginning-of-line
(mmm-add-group 'org-in-draft
               '(
                 (org-in-draft-1
                  :submode org-mode
                  :face mmm-default-submode-face
                  :front "^\\*+ "
                  ;; :back  "^[^*# \n]"
                  :back "^[^*# \n]\\|\\'"
                  :include-front t
                  :end-not-begin t
                  )))

;; Org-mode in here documents
(mmm-add-group 'org-in-here-document
               '(
                 ;; <<EOS
                 ;; org-mode...
                 ;; EOS
                 (org-in-here-document-1
                  :submode org-mode
                  :face mmm-default-submode-face
                  :front "<<[a-zA-Z_][a-zA-Z0-9_]*\n"
                  :back "^[a-zA-Z_][a-zA-Z0-9_]*"
                  :include-front nil
                  :end-not-begin t
                  )
                 ;; """
                 ;; org-mode...
                 ;; """
                 (org-in-here-document-2
                  :submode org-mode
                  :face mmm-default-submode-face
                  :front "^\"\"\"\n"
                  :back  "^\"\"\"\n"
                  :include-front nil
                  :end-not-begin t
                  )
                 ))
