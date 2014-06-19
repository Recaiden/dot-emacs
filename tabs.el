(defun switch-to-next-buffer-with-duplication (&optional window)
  "In WINDOW switch to next buffer.
WINDOW must be a live window and defaults to the selected one.
Return the buffer switched to, nil if no suitable buffer could be
found."
  (interactive)
  (let* ((window (window-normalize-window window t))
	 (frame (window-frame window))
	 (old-buffer (window-buffer window))
	 (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
	 new-buffer entry killed-buffers visible)
    (when (window-minibuffer-p window)
      ;; Don't switch in minibuffer window.
      (unless (setq window (minibuffer-selected-window))
	(error "Window %s is a minibuffer window" window)))

    (when (window-dedicated-p window)
      ;; Don't switch in dedicated window.
      (error "Window %s is dedicated to buffer %s" window old-buffer))

    (catch 'found
      ;; Scan WINDOW's next buffers first.
      (dolist (buffer next-buffers)
	(when (and (or (buffer-live-p buffer)
		       (not (setq killed-buffers
				  (cons buffer killed-buffers))))
		   (not (eq buffer old-buffer))
                   (or (null pred) (funcall pred buffer))
		   (setq entry (assq buffer (window-prev-buffers window))))
	  (setq new-buffer buffer)
	  (set-window-buffer-start-and-point
	   window new-buffer (nth 1 entry) (nth 2 entry))
	  (throw 'found t)))
      ;; Scan the buffer list of WINDOW's frame next, skipping previous
      ;; buffers entries.
      (dolist (buffer (buffer-list frame))
	(when (and (buffer-live-p buffer)
		   (not (eq buffer old-buffer))
                   (or (null pred) (funcall pred buffer))
		   (not (eq (aref (buffer-name buffer) 0) ?\s))
		   (not (assq buffer (window-prev-buffers window))))
	  ))
      ;; Scan WINDOW's reverted previous buffers last (must not use
      ;; nreverse here!)
      (dolist (entry (reverse (window-prev-buffers window)))
	(when (and (setq new-buffer (car entry))
		   (or (buffer-live-p new-buffer)
		       (not (setq killed-buffers
				  (cons new-buffer killed-buffers))))
		   (not (eq new-buffer old-buffer))
                   (or (null pred) (funcall pred new-buffer)))
	  ))

      ;; Show a buffer visible in another window.
      (when visible
	(setq new-buffer visible)
	(set-window-buffer-start-and-point window new-buffer)))

    ;; Remove `new-buffer' from and restore WINDOW's next buffers.
    (set-window-next-buffers window (delq new-buffer next-buffers))

    ;; Remove killed buffers from WINDOW's previous and next buffers.
    (when killed-buffers
      (dolist (buffer killed-buffers)
	(set-window-prev-buffers
	 window (assq-delete-all buffer (window-prev-buffers window)))
	(set-window-next-buffers
	 window (delq buffer (window-next-buffers window)))))

    ;; Return new-buffer.
    new-buffer))


(defun switch-to-prev-buffer-with-duplication (&optional window bury-or-kill)
  "In WINDOW switch to previous buffer.
WINDOW must be a live window and defaults to the selected one.
Return the buffer switched to, nil if no suitable buffer could be
found.

Optional argument BURY-OR-KILL non-nil means the buffer currently
shown in WINDOW is about to be buried or killed and consequently
shall not be switched to in future invocations of this command.

As a special case, if BURY-OR-KILL equals `append', this means to
move the buffer to the end of WINDOW's previous buffers list so a
future invocation of `switch-to-prev-buffer' less likely switches
to it."
  (interactive)
  (let* ((window (window-normalize-window window t))
	 (frame (window-frame window))
	 (old-buffer (window-buffer window))
	 ;; Save this since it's destroyed by `set-window-buffer'.
	 (next-buffers (window-next-buffers window))
         (pred (frame-parameter frame 'buffer-predicate))
	 entry new-buffer killed-buffers visible)
    (when (window-minibuffer-p window)
      ;; Don't switch in minibuffer window.
      (unless (setq window (minibuffer-selected-window))
	(error "Window %s is a minibuffer window" window)))

    (when (window-dedicated-p window)
      ;; Don't switch in dedicated window.
      (error "Window %s is dedicated to buffer %s" window old-buffer))

    (catch 'found
      ;; Scan WINDOW's previous buffers first, skipping entries of next
      ;; buffers.
      (dolist (entry (window-prev-buffers window))
	(when (and (setq new-buffer (car entry))
		   (or (buffer-live-p new-buffer)
		       (not (setq killed-buffers
				  (cons new-buffer killed-buffers))))
		   (not (eq new-buffer old-buffer))
                   (or (null pred) (funcall pred new-buffer))
		   ;; When BURY-OR-KILL is nil, avoid switching to a
		   ;; buffer in WINDOW's next buffers list.
		   (or bury-or-kill (not (memq new-buffer next-buffers))))
	  ))
      ;; Scan reverted buffer list of WINDOW's frame next, skipping
      ;; entries of next buffers.  Note that when we bury or kill a
      ;; buffer we don't reverse the global buffer list to avoid showing
      ;; a buried buffer instead.  Otherwise, we must reverse the global
      ;; buffer list in order to make sure that switching to the
      ;; previous/next buffer traverse it in opposite directions.
      (dolist (buffer (if bury-or-kill
			  (buffer-list frame)
			(nreverse (buffer-list frame))))
	(when (and (buffer-live-p buffer)
		   (not (eq buffer old-buffer))
                   (or (null pred) (funcall pred buffer))
		   (not (eq (aref (buffer-name buffer) 0) ?\s))
		   (or bury-or-kill (not (memq buffer next-buffers))))
	  ))
      (unless bury-or-kill
	;; Scan reverted next buffers last (must not use nreverse
	;; here!).
	(dolist (buffer (reverse next-buffers))
	  ;; Actually, buffer _must_ be live here since otherwise it
	  ;; would have been caught in the scan of previous buffers.
	  (when (and (or (buffer-live-p buffer)
			 (not (setq killed-buffers
				    (cons buffer killed-buffers))))
		     (not (eq buffer old-buffer))
                     (or (null pred) (funcall pred buffer))
		     (setq entry (assq buffer (window-prev-buffers window))))
	    (setq new-buffer buffer)
	    (set-window-buffer-start-and-point
	     window new-buffer (nth 1 entry) (nth 2 entry))
	    (throw 'found t))))

      ;; Show a buffer visible in another window.
      (when visible
	(setq new-buffer visible)
	(set-window-buffer-start-and-point window new-buffer)))

    (if bury-or-kill
	(let ((entry (and (eq bury-or-kill 'append)
			  (assq old-buffer (window-prev-buffers window)))))
	  ;; Remove `old-buffer' from WINDOW's previous and (restored list
	  ;; of) next buffers.
	  (set-window-prev-buffers
	   window (assq-delete-all old-buffer (window-prev-buffers window)))
	  (set-window-next-buffers window (delq old-buffer next-buffers))
	  (when entry
	    ;; Append old-buffer's entry to list of WINDOW's previous
	    ;; buffers so it's less likely to get switched to soon but
	    ;; `display-buffer-in-previous-window' can nevertheless find
	    ;; it.
	    (set-window-prev-buffers
	     window (append (window-prev-buffers window) (list entry)))))
      ;; Move `old-buffer' to head of WINDOW's restored list of next
      ;; buffers.
      (set-window-next-buffers
       window (cons old-buffer (delq old-buffer next-buffers))))

    ;; Remove killed buffers from WINDOW's previous and next buffers.
    (when killed-buffers
      (dolist (buffer killed-buffers)
	(set-window-prev-buffers
	 window (assq-delete-all buffer (window-prev-buffers window)))
	(set-window-next-buffers
	 window (delq buffer (window-next-buffers window)))))

    ;; Return new-buffer.
    new-buffer))


(defun next-buffer-with-duplication ()
  "In selected window switch to next buffer."
  (interactive)
  (cond
   ((window-minibuffer-p)
    (error "Cannot switch buffers in minibuffer window"))
   ((eq (window-dedicated-p) t)
    (error "Window is strongly dedicated to its buffer"))
   (t
    (switch-to-next-buffer))))

(defun previous-buffer-with-duplication ()
  "In selected window switch to previous buffer."
  (interactive)
  (cond
   ((window-minibuffer-p)
    (error "Cannot switch buffers in minibuffer window"))
   ((eq (window-dedicated-p) t)
    (error "Window is strongly dedicated to its buffer"))
   (t
    (switch-to-prev-buffer))))

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer-with-duplication)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer-with-duplication) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer-with-duplication)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer-with-duplication) )))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer-with-duplication)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer-with-duplication) )))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer-with-duplication)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer-with-duplication) )))



;; Tab through buffers
(global-set-key (kbd"<C-tab>") 'next-user-buffer)
(global-set-key (kbd"<C-S-tab>") 'previous-user-buffer)
(global-set-key (kbd"<C-S-iso-lefttab>") 'previous-user-buffer)
(global-set-key (kbd"<C-M-tab>") 'next-user-buffer)
(global-set-key (kbd"<C-M-S-tab>") 'previous-user-buffer)
(global-set-key (kbd"<C-M-S-iso-lefttab>") 'previous-user-buffer)
