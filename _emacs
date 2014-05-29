;;=========================================================================;;
;;=========================================================================;;
;; McIntyre Watts                                                          ;;
;; file: _emacs                                                            ;;
;; Emacs initialization file                                               ;;
;;=========================================================================;;
;;                                                                         ;;
;;=========================================================================;;

;;=========================================================================;;
;;                       C U S T O M I Z A T I O N S                       ;;
;;=========================================================================;;

;; Directory to put various el files.
(add-to-list 'load-path "~/.emacs.d/includes")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/php-mode-1.5.0")
(add-to-list 'load-path "~/.emacs.d/auto-save-list")

;; 
(load "~/.emacs.d/info.el")
(load "~/.emacs.d/disables.el")
(load "~/.emacs.d/rec-colors.el")
(load "~/.emacs.d/multi-buffer.el")
(load "~/.emacs.d/keybinding.el")
(load "~/.emacs.d/modes.el")
(load "~/.emacs.d/customizations.el")
(load "~/.emacs.d/syntax.el")

;; Thanks to David Jolley for the time code
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; Thanks to djbc
(setq cursor-type 'bar)

;; Thanks to Dave Gallucci
;; Comments in italics
;; (Emacs 20)
 (setq w32-enable-italics t)
 (make-face-italic 'font-lock-comment-face)

;; To obtain new font string, place this string:
;; (insert(prin1-to-string(w32-select-font))) in the scratch buffer
;; and hit M-x eval-buffer This will give you the font string.

;;=========================================================================;;
;;                          F U N C T I O N S                              ;;
;;=========================================================================;;

;;ASCII table function
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)  (switch-to-buffer "*ASCII*")  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))  (let ((i 0))
    (while (< i 254)      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))  (beginning-of-buffer))

(defun open-dot-emacs ()
  "opening-dot-emacs"
  (interactive) ;this makes the function a command too
  (find-file "~/.emacs.d/_emacs")
)


;; Thanks to whoever originally wrote this one for cutting responses down
;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;; Thanks to Wojciech Komornicki for this
(defun do-nothing()
  (interactive)
)

;; Thanks to EmacsWiki
(defun indent-or-complete ()
      "Complete if point is at end of a word, otherwise indent line."
      (interactive)
      (if (looking-at "\\>")
          (dabbrev-expand nil)
        (indent-for-tab-command)
        ))

;;=========================================================================;;
;;                        M I S C E L L A N I A                            ;;
;;=========================================================================;;

;(defun MY-XYZ-HOOK ()
;  "My hook for `XYZ-MODE-HOOK'."
;  (local-set-key (kbd "KEY1") 'COMMAND1)
;  (local-set-key (kbd "KEY2") 'COMMAND2)
;  
;  )
;(add-hook 'XYZ-MODE-HOOK 'MY-XYZ-HOOK)

;; make dired recognize German month names

;(if (try-to-load 'dired)
 ;   (setq dired-re-month-and-time
  ;        (concat
   ;        "\\(Jan\\|Feb\\|M.r\\|Apr\\|Ma.\\|June?\\|July?\\|Aug\\|Sep\\|O.t\\|Nov\\|"
;					; June and July are for HP-UX 9.0
 ;          "De.\\) [ 0-3][0-9]\\("
  ;         " [012][0-9]:[0-6][0-9] \\|" ; time
   ;        " [12][90][0-9][0-9] \\|"    ; year on IRIX, NeXT, SunOS, ULTRIX, Apollo,
    ;                                    ; HP-UX, A/UX
     ;      " [12][90][0-9][0-9]  \\)"   ; year on AIX
      ;     )))
