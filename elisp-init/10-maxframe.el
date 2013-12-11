(require 'maxframe)

(setq mf-display-padding-height 47) ;; default is 45

(defvar maxframe-fullscreen-p t "Check if fullscreen is on or off")

(defun maxframe-toggle-fullscreen ()
  (interactive)
  (setq maxframe-fullscreen-p (not maxframe-fullscreen-p))
  (if maxframe-fullscreen-p
          (restore-frame)
        (maximize-frame)))
