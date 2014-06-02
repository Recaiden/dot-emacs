;; Make searches case insensitive
(setq case-fold-search t)

;; Don't add new lines to the end of a file when using down-arrow key
(setq next-line-add-newlines nil)

;; Don't make pesky backup files
(setq make-backup-files nil)

;; split windows should display different buffers
(global-set-key [(control x) \2] 'split-window-switch-buffer)
(global-set-key [(control x) \3] 'hsplit-window-switch-buffer)

;; Thanks to Killian A Foth for the disabling of annoying features
;; Never iconify
(global-unset-key [(control z)])
(global-unset-key [(control x) (control z)])

;; Never quit by mistake.
;; Disabled because of complications with shell-mode
;(global-set-key [(control x) (control c)] 
;  (function 
;   (lambda () (interactive) 
;     (cond ((y-or-n-p "Quit editor? ")
;            (save-buffers-kill-emacs))))))

;; Never switch to overwrite mode, not even accidentally
(global-set-key [insert] 
  (function 
   (lambda () (interactive) 
     (message "Sorry, overwrite mode has been disabled forever."))))

;; Fix Home and End
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)


; make completion buffers disappear after 3 seconds.
(add-hook 'completion-setup-hook
  (lambda () (run-at-time 15 nil
    (lambda () (delete-windows-on "*Completions*")))))


