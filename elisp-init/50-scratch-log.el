;;(add-private-load-path "scratch-log")
(require 'scratch-log)

(setq sl-scratch-log-file "~/.emacs.d/scratch-log")
(setq sl-prev-scratch-string-file "~/.emacs.d/.scratch-log-prev")
(setq sl-restore-scratch-p t)
(setq sl-prohibit-kill-scratch-buffer-p t)
(setq sl-use-timer t)
(setq sl-timer-interval 30) ;  Seconds of timer interval.
