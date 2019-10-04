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

;;packaging
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

; Tree viewer for files
(require 'neotree)
(require 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(neotree-show)

(require 'magit)

(load "~/.emacs.d/private.el") ; contains per-machine keys and shouldn't be uploaded
(load "~/.emacs.d/info.el")
(load "~/.emacs.d/disables.el")
(load "~/.emacs.d/rec-colors.el")
(load "~/.emacs.d/multi-buffer.el")
(load "~/.emacs.d/keybinding.el")
(load "~/.emacs.d/modes.el")
(load "~/.emacs.d/customizations.el")
(load "~/.emacs.d/syntax.el")
(load "~/.emacs.d/tabs.el")

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

(auto-fill-mode -1)

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
	
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
	(end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
	  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
	(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun copy-to-line-end (arg)
  "Copy text into the kill ring from the position of the cursor to the end of the line.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines.  Probably."
  (interactive "p")
  (let ((beg (point))
	(end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
	  (setq beg (save-excursion (goto-char (mark)) (point)))
	(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (kill-ring-save beg end))
  (kill-append "\n" nil)
  (message "copied")
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; Shell within emacs
;(setq explicit-shell-file-name "/usr/bin/python")
;(setq shell-file-name "python")
;(setq explicit-python-args '("/home/mlwatts/pyshell/pyshell.py"))
;(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

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

(shell)
(rename-buffer "shell")
