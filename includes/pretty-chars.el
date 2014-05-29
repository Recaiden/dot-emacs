;;; pretty-lambda.el --- Show the word `chars' as the Greek letter.
;;
;; Filename: pretty-chars.el
;; Description: Show the word `chars' as the Greek letter.
;; Author: Drew Adams
;;         See http://www.emacswiki.org/emacs/PrettyChars for the original
;;         code snippet and its history.
;; Maintainer: Drew Adams
;; Copyright (C) 2009-2012, Drew Adams, all rights reserved.
;; Created: Sun Jun 14 11:07:04 2009 (-0700)
;; Version: 22.0
;; Last-Updated: Sun Jan  1 14:41:08 2012 (-0800)
;;           By: dradams
;;     Update #: 143
;; URL: http://www.emacswiki.org/cgi-bin/wiki/pretty-chars.el
;; Keywords: convenience display
;; Compatibility: GNU Emacs: 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Whenever "chars" appears as a separate word, it is displayed using the
;;  Greek letter.
;;
;;  Put this in your init file (~/.emacs), to turn this display on for
;;  the modes in `pretty-chars-auto-modes', which by default are the
;;  usual Lisp modes:
;;
;;   (require 'pretty-chars)
;;   (pretty-chars-for-modes)
;;
;;  You can toggle pretty-chars display on/off in any buffer, using
;;  command `pretty-chars-mode'.  Use `global-pretty-chars-mode' to
;;  toggle the display in all buffers.
;;
;;  Three alternative ways to turn on pretty-chars display for a
;;  specific buffer mode:
;;
;;  1. (add-hook 'my-mode-hook 'turn-on-pretty-chars-mode)
;;  2. (pretty-chars 'my-mode)
;;  3. (add-hook 'my-mode-hook 'pretty-chars)
;;
;;  The first way uses minor mode `pretty-chars-mode', so you can
;;  easily toggle pretty-chars display.  The last two just turn on
;;  the display.  To turn it off, use `turn-off-pretty-chars-mode'.
;;
;;
;;  User options defined here:
;;
;;    `global-pretty-chars-mode', `pretty-chars-auto-modes',
;;    `pretty-chars-mode'.
;;
;;  Commands defined here:
;;
;;    `global-pretty-chars-mode', `pretty-chars-for-modes',
;;    `pretty-chars-mode'
;;
;;  Non-interactive functions defined here:
;;
;;    `pretty-chars', `turn-off-pretty-chars-mode',
;;    `turn-on-pretty-chars-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/04 dams
;;     Added autoload cookies (for defgroup, defcustom, and commands).
;; 2009/06/14 dams
;;     Added: group pretty-chars, (global-)pretty-chars-mode,
;;            pretty-chars-for-modes, pretty-chars-auto-modes,
;;            turn-(on|off)-pretty-chars-mode.
;;     pretty-chars: Added optional MODE arg (required arg in some existing code).
;;     Created from code snippet at http://www.emacswiki.org/emacs/PrettyChars.
;;
;;     Called this pretty-chars.el, where the last "da" is Drew Adams.
;;     Luke Gorrie has already used the name pretty-chars.el:
;;     http://fresh.homeunix.net/~luke/misc/emacs/pretty-chars.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(defgroup pretty-chars nil
  "Display of the word `chars' as the Greek character."
    :group 'convenience :group 'programming)

;;;###autoload
(define-minor-mode pretty-chars-mode 
    "Buffer-local minor mode to display the word `chars' as the Greek letter.
With ARG, turn mode on if ARG is positive, off otherwise."
    :init-value nil
    :sear-string "lambda"
    :repl-alphabet 'greek-iso8859-7
    :repl-char 107
    (cond (pretty-chars-mode
	   (pretty-chars 'sear-string 'repl-alphabet 'repl-char)
	   (font-lock-fontify-buffer))
	  (t
	   (font-lock-remove-keywords
	    nil `(((concatenate 'string "\\<" 'sear-string  "\\>")
		   (0 (progn (compose-region (match-beginning 0) (match-end 0)
					     ,(make-char 'repl-alphabet 'repl-char))
			     nil)))))
	   (save-excursion
	     (goto-char (point-min))
	     (while (re-search-forward "\\<chars\\>" nil t)
	       (decompose-region (match-beginning 0) (match-end 0)))))))

;;;###autoload

;; This was originally from <URL: http://www.emacswiki.org/emacs/PrettyChars>.
;; See that page for the history of this code snippet.  I just added MODE as an
;; optional argument.
(defun pretty-chars (s-string r-alphabet r-char &optional mode)
  "Display the word `chars' as the Greek letter.
Non-nil optional arg means use pretty-chars display in that MODE.
nil means use pretty-chars display for the current mode."
  (font-lock-add-keywords
   mode `(("\\<lambda\\>";(concatenate 'string "\\<" "lambda" "\\>")
   (0 (progn (compose-region (match-beginning 0) (match-end 0)
        ,(make-char 'greek-iso8859-7 107))
      nil))))))

(defun turn-on-pretty-chars-mode (s-string r-alphabet r-char) 
  (pretty-chars-mode 1))
(defun turn-off-pretty-chars-mode () (pretty-chars-mode -1))

;;;;

(provide 'pretty-chars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pretty-chars.el ends here


;; global-pretty-char-mode - > turn-on-pretty-char-mode - > pretty-char-mode 1 -> pretty-chars