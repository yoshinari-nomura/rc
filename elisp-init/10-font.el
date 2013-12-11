;;
;; Font settings for emacs 24
;; cf. http://d.hatena.ne.jp/setoryohei/20110117/1295336454
;;

(defun make-font-set (fontset-name size asciifont jpfont)
  (let* ((font (format "%s-%d:weight=normal:slant=normal" asciifont size))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont))
         (fsn nil))
    (if fontset-name
        (setq fsn (create-fontset-from-ascii-font font nil fontset-name))
      (set-face-attribute 'default nil :family asciifont :height (* size 10)))
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec) ; 漢字(JIS0213 1面)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec) ; 漢字(JIS0213 2面)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font fsn '(#x0080 . #x024F) fontspec)    ; 分音符付きラテン

    ;; ギリシャ文字: 全角幅にするなら jp-fontspec，半角幅なら fontspec
    (set-fontset-font fsn '(#x0370 . #x03FF) jp-fontspec)

    ;; 一部の East Asian Ambiguous Width Character は，
    ;; フォントによっては半角幅で表示される(ヒラギノとか)．
    ;; そのような文字には Osaka を使う．
    (set-fontset-font fsn '(?× . ?×) "Osaka")

    (or fsn (face-attribute 'default :fontset))
    ))

(when (and (>= emacs-major-version 24)
           (eq window-system 'ns))

  ;; 好きなフォントの名前を調べるには， *scratch* で
  ;;   (mapc (lambda (x) (insert (format "%s\n" x ))) (x-list-fonts "*"))
  ;; とやって，
  ;;   -apple-Hiragino_Mincho_ProN-medium-normal-normal-*-*-*-*-*-p-0-iso10646-1
  ;; みたいなのが出たら，Hiragino_Mincho_ProN の部分を使えばいい．
  ;; アンダースコア "_" は，スペース " " に変える．"MS Gothic" とか，"Osaka" とか．
  ;;
  (setq my-default-fontset (make-font-set "myfonts" 14 "Menlo" "Hiragino Maru Gothic ProN"))

  ;; デフォルトのフレームパラメータでフォントセットを指定
  (add-to-list 'default-frame-alist `(font . ,my-default-fontset))

  ;; フォントサイズの比を設定
  (dolist (elt '(("^-apple-hiragino.*"               . 1.2)
                 (".*osaka-bold.*"                   . 1.2)
                 (".*osaka-medium.*"                 . 1.2)
                 (".*courier-bold-.*-mac-roman"      . 1.0)
                 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                 (".*monaco-bold-.*-mac-roman"       . 0.9)))
    (add-to-list 'face-font-rescale-alist elt))
  )

;;
;; for emacs 23
;;
(when (and (= emacs-major-version 23)
           (eq window-system 'ns))
  (let ((my-font
         (if (< 768 (x-display-pixel-height))
             "-*-*-medium-r-normal--16-*-*-*-*-*-fontset-hiramaru"
           "-*-*-medium-r-normal--14-*-*-*-*-*-fontset-hiramaru")))
    (create-fontset-from-ascii-font
     "Menlo-14:weight=normal:slant=normal" nil "menlomarugo")
    (set-fontset-font "fontset-menlomarugo"
                      'unicode
                      (font-spec :family "Hiragino Maru Gothic ProN" :size 16)
                      nil
                      'append)
    (add-to-list 'default-frame-alist '(font . "fontset-menlomarugo"))
    (when (= emacs-major-version 22)
      (require 'carbon-font)
      ;; hirakaku_w3 hirakaku_w6 hirakaku_w8 hiramin_w3 hiramin_w6
      ;; osaka hiramaru
      ;; 7, 8, 9, 10, 12, 14, 16, 18, 20, 24
      (fixed-width-set-fontset "hiramaru"
                               (if (< 800 (x-display-pixel-height))
                                   16 12
                                   )))))
