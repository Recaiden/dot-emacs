;;;###autoload
(defgroup pretty-delta nil
  "Display of the word `delta' as the Greek character."
    :group 'convenience :group 'programming)

;;;###autoload
(defcustom pretty-delta-auto-modes
  '(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "*Modes affected by `pretty-delta-for-modes'."
  :type '(repeat symbol) :group 'pretty-delta)

;;;###autoload
(defun pretty-delta-for-modes (&optional turn-off)
  "Use `pretty-delta-mode' for modes in `pretty-delta-auto-modes'.
`C-u' to turn off."
  (interactive "P")
  (let (hook-var)
    (cond (turn-off
           (dolist (m  pretty-delta-auto-modes)
             (remove-hook (setq hook-var (intern (concat (symbol-name m) "-hook")))
                          'turn-on-pretty-delta-mode)
             (add-hook hook-var 'turn-off-pretty-delta-mode))
           (when (memq major-mode pretty-delta-auto-modes)
             (turn-off-pretty-delta-mode))) ; Current buffer
          (t
           (dolist (m  pretty-delta-auto-modes)
             (remove-hook (setq hook-var (intern (concat (symbol-name m) "-hook")))
                          'turn-off-pretty-delta-mode)
             (add-hook hook-var 'turn-on-pretty-delta-mode))
           (when (memq major-mode pretty-delta-auto-modes)
             (turn-on-pretty-delta-mode)))))) ; Current buffer

;;;###autoload
(define-minor-mode pretty-delta-mode
    "Buffer-local minor mode to display the word `delta' as the Greek letter.
With ARG, turn mode on if ARG is positive, off otherwise."
  :init-value nil
  (cond (pretty-delta-mode
         (pretty-delta)
         (font-lock-fontify-buffer))
        (t
         (font-lock-remove-keywords
          nil `(("\\<delta\\>"
                 (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                           ,(make-char 'greek-iso8859-7 68))
                           nil)))))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "\\<delta\\>" nil t)
             (decompose-region (match-beginning 0) (match-end 0)))))))

;;;###autoload
(define-globalized-minor-mode global-pretty-delta-mode
    pretty-delta-mode turn-on-pretty-delta-mode
    "Global minor mode to display the word `delta' as the Greek letter.
With ARG, turn mode on if ARG is positive, off otherwise.")

;; This was originally from <URL: http://www.emacswiki.org/emacs/PrettyDelta>.
;; See that page for the history of this code snippet.  I just added MODE as an
;; optional argument.
(defun pretty-delta (&optional mode)
  "Display the word `delta' as the Greek letter.
Non-nil optional arg means use pretty-delta display in that MODE.
nil means use pretty-delta display for the current mode."
  (font-lock-add-keywords
   mode `(("\\<delta\\>"
   (0 (progn (compose-region (match-beginning 0) (match-end 0)
        ,(make-char 'greek-iso8859-7 68))
      nil))))))

(defun turn-on-pretty-delta-mode  () (pretty-delta-mode  1))
(defun turn-off-pretty-delta-mode () (pretty-delta-mode -1))

;;;;

(provide 'pretty-delta)