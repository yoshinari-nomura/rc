;;;
;;; load path and directory settings.
;;;

(setq emacs-exec-path
      '("~/sys/lib/elisp/mew/current/bin"
        "~/sys/lib/elisp/migemo/bin"))

(setq shell-exec-path
      (split-string
       (with-temp-buffer
         (call-process
          (getenv "SHELL") nil '(t nil) nil "-c" "echo -n $PATH")
         (buffer-substring-no-properties (point-min) (point-max)))
       ":"))

(dolist (ent (reverse (append emacs-exec-path shell-exec-path)))
  (let ((dir (expand-file-name ent)))
    (when (and (file-exists-p dir) (not (member dir exec-path)))
      (setq exec-path (cons dir exec-path))
      (setenv "PATH" (concat dir ":" (getenv "PATH"))))))
