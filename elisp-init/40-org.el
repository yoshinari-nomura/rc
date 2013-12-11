(require 'org)
(require 'org-mac-link) ;; from contrib for org-mac-grab-link
(require 'orglue)
(require 'orglue-anything-config)
(require 'org-mew)

(setq orglue-org-project-file "~/prj/private/org/TODO.org")

;;; orglue
(global-set-key "\C-x\C-i" 'orglue-indent-rigidly-to-current-level)

;;
;; base directory settings
;;

(setq org-directory "~/prj/private/org")
(setq org-agenda-files (expand-file-name "agenda-files.list" org-directory))

;;
;; keywords for TODO
;;

(setq org-todo-keywords
      '(("STARTED(s)" "TODO(t)" "MAYBE(m)" "WAIT(w)" "APPT(a)" "|"
         "DONE(d)" "SOMEDAY(S)" "CANCELLED(c)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearances

(setq org-startup-folded t)
(setq org-hide-leading-stars t)
;; (setq org-odd-levels-only t)
;; #+STARTUP: odd
;; #+STARTUP: oddeven

; Number of empty lines needed to keep an empty line between
; collapsed trees.  Special case: when 0, never leave empty lines in
; collapsed view.
; default is 2.
(setq org-cycle-separator-lines 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom face for org-hide-leading-stars.

(custom-set-faces
 '(org-hide
   (
    (((class color) (background light))
     (:foreground "lavender"))
    (((class color) (background dark))
     (:foreground "black")))))

(setq org-todo-keyword-faces
      '(
        ("STARTED"   :foreground "blue"         :weight bold)
        ("TODO"      :foreground "red"          :weight bold)
        ("MAYBE"     :foreground "brown"        :weight bold)
        ("WAIT"      :foreground "orange"       :weight bold)
        ("APPT"      :foreground "gray"         :weight bold)
        ("DONE"      :foreground "forest green" :weight bold)
        ("SOMEDAY"   :foreground "magenta"      :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ))

(setq org-tag-faces
      '(("@OFFICE"   :foreground "yellow"       :weight bold)
        ("@HOME"     :foreground "blue"         :weight bold)
        ("@LABO"     :foreground "orange"       :weight bold)
        ("@CITY"     :foreground "forest green" :weight bold)
        ("@TRIP"     :foreground "magenta"      :weight bold)
        ("@JIFFY"    :foreground "forest green" :weight bold)
        ("PROJECT"   :foreground "forest green" :weight bold)
        ("INCOMING"  :foreground "orange"       :weight bold)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For fast editing

; make C-c-C-t direct todo-selection rather than toggle-behavior.
; default is t since 2009-02-20
(setq org-use-fast-todo-selection t)

; C-a and C-e ignore heading stars or trailing tags.
(setq org-special-ctrl-a/e t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tables
;;

;; Non-nil means automatically blank table field when starting to type
;; into it.  This only happens when typing immediately after a field
;; motion command (TAB, S-TAB or RET).
(setq org-table-auto-blank-field t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto mode

(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key binding

(add-hook 'org-mode-hook
          (lambda ()
            ;; (define-key org-mode-map "\C-oi" 'org-mac-grab-link)
            (define-key org-mode-map "\M-j"  'org-metadown)
            (define-key org-mode-map "\M-h"  'org-metaleft)
            (define-key org-mode-map "\M-l"  'org-metaright)
            (define-key org-mode-map "\M-k"  'org-metaup)
            (define-key org-mode-map "\M-t"  'org-toggle-qa)
            ;; (make-variable-buffer-local 'coding-system-for-write)
            ;; (setq coding-system-for-write 'utf-8)
            (auto-fill-mode -1)
            ))

;;; outline-mode

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode 1)
            ;; (turn-on-orgstruct++)
            (define-key outline-minor-mode-map "\C-i" 'org-cycle)
            (define-key outline-minor-mode-map "\C-c\C-f" 'outline-forward-same-level)
            (define-key outline-minor-mode-map "\C-c\C-b" 'outline-backward-same-level)
            (define-key outline-minor-mode-map "\C-c\C-n" 'outline-next-visible-heading)
            (define-key outline-minor-mode-map "\C-c\C-p" 'outline-previous-visible-heading)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-capture (new framework to replace the org-remember)

(setq org-capture-templates
      '(
        ("t" "Todo" entry
         (file+headline "~/prj/private/org/TODO.org" "Inbox")
         "***** TODO %^{Title}%?\n %i\n %a")

        ("m" "Mew" entry
         (file+headline "~/prj/private/org/TODO.org" "Inbox")
         "***** TODO %?%:subject\n      %a")

        ("j" "Journal" entry
         (file+headline "~/prj/private/org/TODO.org" "Journal")
         "* %U %?\n\n %i\n %a")

       ("i" "Idea" entry
        (file+headline "~/prj/private/org/TODO.org" "Idea")
        "** SOMEDAY %^{Title}%?\n %i\n %a")

       ("p" "Project" entry
        (file+headline "~/prj/private/org/TODO.org" "Projects")
        "** %^{Title}%? :PROJECT:\n %i\n %a")
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom agenda views

; (setq org-agenda-cmp-user-defined 'org-agenda-cmp-user-defined)
; (defun org-agenda-cmp-user-defined (a b))

(setq org-stuck-projects
      '("+LEVEL=2-MHC-IDEA" ("TODO" "STARTED" "WAIT" "APPT")
        ("MHC" "Material" "Schedule")))

(setq org-agenda-deadline-leaders  '("    締切" "あと%02d日"))
(setq org-agenda-scheduled-leaders '("    当日" "%02d日経過"))
(setq org-agenda-todo-keyword-format "%7s")
(setq org-agenda-use-time-grid nil)
(setq org-agenda-weekend-days '(0)) ;; 0 is sunday, default: '(6 0)
(setq org-category "")
(setq org-agenda-include-inactive-timestamps t)
(setq org-agenda-inactive-leader "         ")
(setq org-agenda-prefix-format
      '(
        (agenda    . "  %?-8:c%?-12t% s")
        (timeline  . "    %? s")
        (todo      . "  %?-8:b")
        (tags      . "  %?-8:c")
        (search    . "  %?-8:c")))

(setq org-agenda-custom-commands
      '(("p" "Projects"
         tags "LEVEL=2+PROJECT"
         ((org-agenda-prefix-format "%(orglue-make-link-to-project-top)  ")))
        (" " "My Standard Agenda View"
         ((agenda "")
          (tags-todo "INCOMING"
                ((org-agenda-overriding-header "Incoming")
                 (org-agenda-show-inherited-tags nil)
                 (org-agenda-sorting-strategy '(todo-state-up priority-down time-up))))
          (tags-todo "-INCOMING-@HOME-@SHOP-@TRIP-@MEETING-@BOSS-HABITS"
                     ((org-agenda-overriding-header "Tasks")
                      (org-agenda-sorting-strategy '(todo-state-up priority-down time-up))))
          (tags "LEVEL=2+PROJECT"
                ((org-agenda-overriding-header "Projects")
                 (org-agenda-show-inherited-tags nil)
                 (org-agenda-prefix-format "%(orglue-make-link-to-project-top)  ")))))
        ("O" . "My Original Commands")
        ("Oa" "ALL"
         tags-todo "-HIDDEN"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down time-up))))
        ("Oo" "OFFICE"
         tags-todo "-@HOME-@SHOP-@TRIP-@MEETING-@BOSS-HABITS"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down time-up))))
        ("Om" "MEETING"
         tags-todo "@MEETING"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down time-up))))
        ("Oh" "HOME"
         tags-todo "@HOME"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down time-up))))
        ("Os" "SHOP"
         tags-todo "@SHOP"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down time-up))))
        ("Ot" "TRIP"
         tags-todo "@TRIP"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down time-up))))
        ))

(setq org-default-priority ?C)
(setq org-priority-start-cycle-with-default nil)
(setq org-priority-faces '((?A (:foreground "red"    :weight bold :underline nil))
                           (?B (:foreground "blue" :weight bold :underline nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agenda view for japanese mhc users

(defun org-agenda-format-date-aligned-jp (date)
  (let ((calendar-day-abbrev-array
         ["日" "月" "火" "水" "木" "金" "土"]
         ;; ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
         )
        (date-string))
    (setq date-string
          (format "%02d/%02d %s"
                  ;; (nth 2 date)                  ; year
                  (nth 0 date)                     ; month
                  (cadr date)                      ; day
                  (calendar-day-name date t)       ; week
                  ))
    ;; TODO: mhc-summary-face-default-today
    ;;       mhc-category-face-holiday
    (put-text-property 0 (length date-string) 'face
                       (cond
                        ((eq (calendar-day-of-week date) 6)
                         'mhc-summary-face-saturday)
                        ((eq (calendar-day-of-week date) 0)
                         'mhc-summary-face-sunday)
                        (t
                         'mhc-summary-face-default))
                       date-string)
    date-string))

(setq org-agenda-format-date 'org-agenda-format-date-aligned-jp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc settings. I don't undestand how they work :)

; (setq org-completion-use-ido t)
; (setq org-refile-targets
;       (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
; (setq org-refile-use-outline-path 'file)
; (setq org-refile-targets  '((org-agenda-files :level . 2)))

(setq org-refile-use-outline-path 'path)
(setq org-outline-path-complete-in-steps nil) ;; nil is suitable for anything.el
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-reverse-note-order t)
(setq org-refile-targets
      '(
        ;; (org-agenda-files :maxlevel . 2)
        (nil :maxlevel . 2)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MobileOrg

; Staging Area -- IX point on Dropbox
(setq org-mobile-directory "~/Dropbox/MobileOrg")

; Mobileorg appends captured entries and pointers to flagged and
; changed entries to the file.
; Dropbox server.
; (defconst org-mobile-capture-file "mobileorg.org")

(setq org-mobile-inbox-for-pull  (concat org-directory "/from-mobile.org"))
(setq org-mobile-agendas '("Oo" "Oh" "Os" "Ot"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel settings since Org mode 7.0

(setq org-link-search-must-match-exact-headline nil) ;; for #+INCLUDE:

(setq org-ditaa-jar-path (private-load-path
                          "org-mode/contrib/scripts/jditaa.jar"))
(setq org-babel-default-header-args:ditaa
      '(
        (:results . "file")
        (:exports . "results")
        (:java . "-Dfile.encoding=UTF-8 -Djava.awt.headless=true")))

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (C . t)
   (haskell . t)
   (ocaml . nil)
   (python . t)
   (ruby . t)
   (screen . nil)
   (sh . t)
   (sql . nil)
   (sqlite . t)))

;; use runhaskell when :results is output
(defadvice org-babel-haskell-initiate-session
  (around org-babel-haskell-initiate-session-advice)
  (let* ((buff (get-buffer "*haskell*"))
         (proc (if buff (get-buffer-process buff)))
         (type (cdr (assoc :result-type params)))
         (haskell-program-name
          (if (equal type 'output) "runhaskell-ob" "ghci")))
    (if proc (kill-process proc))
    (sit-for 0)
    (if buff (kill-buffer buff))
    ad-do-it))
(ad-activate 'org-babel-haskell-initiate-session)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  new exporter -- html

(setq org-export-coding-system 'utf-8)
(setq org-export-default-language "ja")
(setq org-export-dictionary
      '(("Author")
        ("Date")
        ("Equation")
        ("Figure")
        ("Footnotes")
        ("List of Listings")
        ("List of Tables")
        ("Listing %d:")
        ("Listing %d: %s")
        ("See section %s")
        ("Table %d:")
        ("Table %d: %s")
        ("Table of Contents")
        ("Unknown reference")))

(eval-after-load 'org
  '(progn
     (setq org-emphasis-regexp-components
           (list
            ;; pre-match
            (concat (nth 0 org-emphasis-regexp-components) "　，（")
            ;; post-match
            (concat (nth 1 org-emphasis-regexp-components) "　．，）")
            ;; border
            (nth 2 org-emphasis-regexp-components)
            ;; body-regexp
            (nth 3 org-emphasis-regexp-components)
            ;; newline
            (nth 4 org-emphasis-regexp-components)))
     (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)))


(setq org-html-validation-link nil)

;; "<table border="2" cellspacing="0"
;;    cellpadding="6" rules="groups" frame="hsides">"
(setq org-html-table-tag "<table>")

(setq org-html-head
"<style type=\"text/css\">
<!--/*--><![CDATA[/*><!--*/
html {
    font-family:'ヒラギノ丸ゴ Pro','Hiragino Maru Gothic Pro','メイリオ',Meiryo,'ＭＳ Ｐゴシック',sans-serif;
    font-size: 12pt;
}
pre {
    border: 1pt solid #AEBDCC;
   /* background-color: #F3F5F7; */
    background-color: #F8F8F8;
    padding: 5pt;
    font-family: courier, monospace;
    font-size: 90%;
    overflow:auto;
}
table, th, td {
    border: 1px #000000 solid;
    border-collapse: collapse;
}
td, th {
    vertical-align: top;
}
dt {
    font-weight: bold;
    color: green;
}
div.figure {
    padding: 0.5em;
}
div.figure p {
    text-align: center;
}
.linenr {
    font-size: smaller;
}
.code-highlighted {
    background-color:#ffff00;
}

/* my settings */

body {
    color: black;
    // background: #d1eeee;
    color: black;
    line-height: 1.5;
    margin-top: 50px;
    margin-right: 10%;
    margin-left: 1em;
    letter-spacing: 2px;
}

h1 {
    text-align: center;
}

pre {
    padding: 10px 10px 10px 10px;
    margin: 0px 0px 0px 1em;
    letter-spacing: 0px;
    line-height: 1.2;
}

h2 {
    margin-bottom: 2em;
    border: solid;
    padding: 0px 10px 2px 10px;
    border-width: 0px 0px 3px 0.7em;
    border-color: transparent transparent black black;
}

h3 {
    margin-bottom: 2em;
    border: solid;
    padding: 0px 10px 2px 10px;
    border-width: 0px 0px 1px 0.7em;
    border-color: transparent transparent brown brown;
}

h4 {
    border: solid;
    padding: 0px 10px 2px 10px;
    border-width: 0px 0px 1px 0.7em;
    border-color: blue;
}

h5 {
    border: solid;
    padding: 0px 10px 2px 10px;
    border-width: 0px 0px 1px 0.7em;
    border-color: green;
    color: green;
}

th {
    font-weight: bold;
    border: 1px solid #777777;
    background-color: #8899FF;
    padding: 3px;
    border-bottom: 0px;
    border-right: 0px;
}

td {
    border-top: 1px solid #777777;
    padding: 3px;
}

tr {
    background-color: aliceblue;
}

table {
    border-spacing: 0px;
    empty-cells:show;
    margin-bottom: 6px;
    border: 1px solid #0000AA;
    border-top: 0px;
}

table.layer {
    text-align: center;
}

blockquote {
    border: 1pt solid #AEBDCC;
    background-color: #F3F5F7;
    padding: 0px 0px 0px 1em;
}

td.byteorder {
    text-align: center;
}

em {
    color: red;
}

b {
    color: green;
}

img {
    border: 1pt solid #AEBDCC;
    background-color: #F3F5F7;
}

/* postamble */
div#postamble {
    width: 100%;
    text-align: right;
    font-size: smaller;
    border-top: 2px solid #808080;
    margin-top: 3em;
    padding: 1em 0em;
}
div#postamble p {
    margin: 0.2em 0em;
}
p.creator {
    color: #a0a0a0;
}
/*]]>*/-->
</style>
<link rel=\"stylesheet\" type=\"text/css\" href=\"pub/css/text.css\">
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new exporter -- latex

(setq org-latex-default-class "jsarticle")

(require 'ox-latex)
(add-to-list
 'org-latex-classes
 '("jarticle"
   "\\documentclass[a4j]{jarticle}"
   ("\\vspace{-2em}\\section{%s}" . "\\vspace{-2em}\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
   ))
(add-to-list
 'org-latex-classes
 '("jsarticle"
   "\\documentclass[a4j]{jsarticle}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}"    . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}"     . "\\paragraph*{%s}")
   ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")
   ))

(setq org-latex-date-timestamp-format "%Y-%m-%d")

(setq org-latex-pdf-process
  '(
    "platex -interaction nonstopmode -output-directory %o %f"
    "platex -interaction nonstopmode -output-directory %o %f"
    "platex -interaction nonstopmode -output-directory %o %f"
    "dvipdfmx %b.dvi"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use listings package for listing source code.
;;
;; http://orgmode.org/worg/org-tutorials/org-latex-export.html
;;   12.4 Example listings setup
;; http://mytexpert.sourceforge.jp/index.php?Listings
;;   Listings - MyTeXpert
;;
(setq org-latex-listings t)

(setq org-latex-listings-options
      '(("frame"       "lines")
        ("xleftmargin" "3zw")
        ("basicstyle"  "{\\ttfamily\\footnotesize}")
        ("lineskip"    "-0.5ex")
        ("numbers"     "left")
        ("numberstyle" "\\tiny")))

(setq org-latex-remove-logfiles nil)

(org-add-link-type
 "latex" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span class=\"%s\">%s</span>" path desc))
    ((eq format 'latex)
     (format "\\%s{%s}" path desc)))))

;; Insert template is gone since 8.0
(defun org-insert-export-options-template-generic ()
  (interactive)
  (insert "#+TITLE:
#+AUTHOR:
#+EMAIL:
#+DATE:
#+OPTIONS: H:3 num:2 toc:nil
#+OPTIONS: ^:nil @:t \\n:nil ::t |:t f:t TeX:t
#+OPTIONS: skip:nil
#+OPTIONS: author:t
#+OPTIONS: email:nil
#+OPTIONS: creator:nil
#+OPTIONS: timestamp:nil
#+OPTIONS: timestamps:nil
#+OPTIONS: d:nil
#+OPTIONS: tags:t
#+TEXT:
#+DESCRIPTION:
#+KEYWORDS:
#+LANGUAGE: ja
#+STARTUP: odd
#+LATEX_CLASS: jsarticle
#+LATEX_CLASS_OPTIONS: [a4j]
# #+LATEX_HEADER: \\usepackage{plain-article}
# #+LATEX_HEADER: \\renewcommand\\maketitle{}
# #+LATEX_HEADER: \\pagestyle{empty}
# #+LaTeX: \\thispagestyle{empty}
"))

; add dvipdfmx option to graphicx
(setq org-latex-default-packages-alist
      '(("AUTO"     "inputenc"  t)
        ("T1"       "fontenc"   t)
        (""         "fixltx2e"  nil)
        ("dvipdfmx" "graphicx"  t)
        (""         "longtable" nil)
        (""         "float"     nil)
        (""         "wrapfig"   nil)
        (""         "soul"      t)
        (""         "textcomp"  t)
        (""         "marvosym"  t)
        (""         "wasysym"   t)
        (""         "latexsym"  t)
        (""         "amssymb"   t)
        (""         "hyperref"  nil)
        "\\tolerance=1000"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small helper for making Q and A page

(defun insert-at (string col)
  (move-to-column col t)
  (insert string))

(defvar org-qa-default-column 6)

(defun org-qa-set-default-column (&optional col)
  (interactive)
  (setq org-qa-default-column (or col (current-column)))
  (message "setq qa-default-column to %d" org-qa-default-column))

(defun org-qa-insert (&optional start-col)
  (interactive)
  (let ((start-col (or start-col org-qa-default-column)))
    (insert-at "+ Q :: \n" start-col)
    (insert-at "       " start-col)
    (set-mark (point))
    (insert "\n\n")
    (insert-at "+ A :: \n" start-col)
    (insert-at "       \n\n" start-col)))

(defun org-toggle-qa ()
  (interactive)
  (save-excursion
    (let ((eol (progn (move-end-of-line nil) (point)))
          (bol (progn (move-beginning-of-line nil) (point))))
      (when (search-forward-regexp "^  *\\+" eol t)
        (backward-char)
        (cond
         ((looking-at "\\+ Q :: ")
          (replace-match "+ A :: "))
         ((looking-at "\\+ A :: ")
          (replace-match "+ "))
         ((looking-at "\\+ *")
          (replace-match "+ Q :: ")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clocking

(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-into-drawer t)
(setq org-clock-history-length 50)
(org-clock-persistence-insinuate)
