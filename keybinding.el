;;=========================================================================;;
;;                      K E Y   B I N D I N G S                            ;;
;;=========================================================================;;

;;Key bindings
(define-key global-map (kbd"<f1>") 'goto-line)
;(define-key global-map [(shift f1)] ')

(define-key global-map (kbd"<f2>") 'color-theme-aliceblue)
(define-key global-map [(shift f2)] 'color-theme-xenburn)
(define-key global-map [(ctrl f2)] 'zenburn)

;(define-key global-map (kbd"<f3>") ')
;(define-key global-map [(shift f3)] ')
;(define-key global-map (kbd"<f4>") ')

;(define-key global-map [(shift f4)] ')

(define-key global-map (kbd "<f5>") 'save-buffer)
;(define-key global-map [(shift f5)] ')
(define-key global-map [(ctrl f5)] 'eval-region)

;(define-key global-map (kbd"<f6>") ')
;(define-key global-map [(shift f6)] ')

;(define-key global-map (kbd"<f7>") ')
;(define-key global-map [(shift f7)] ')

(define-key global-map (kbd"<f8>") 'neotree-toggle)
;(define-key global-map (kbd"<f8>") ')
;(define-key global-map [(shift f8)] ')

;(define-key global-map (kbd"<f9>") ')
;(define-key global-map [(shift f9)] ')

;(define-key global-map (kbd"<f10>") ')
;(define-key global-map [(shift f10)] ')

;(define-key global-map (kbd"<f11>") ')
;(define-key global-map [(shift f11)] ')

;(define-key global-map (kbd"<f12>") ')
(define-key global-map [(shift f12)] 'open-dot-emacs)

(define-key global-map "\C-s" 'save-buffer)

(global-unset-key "\C-j")
(define-key global-map "\C-j" 'copy-to-line-end)

(global-unset-key "\C-a")
(define-key global-map "\C-a" 'mark-whole-buffer)

;; Non-standard form because the semi-colon is weird
(global-set-key [(control ?\;)] 'backward-kill-word)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Make Control and Page Up/Page Down scroll the other buffer
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)

;; Make search use Ctrl+f and Ctrl+Shift+f for reverse like a reasonable app
(define-key global-map (kbd "C-f") 'isearch-forward)
(define-key global-map (kbd "C-S-f") 'isearch-backward)
(add-hook 'isearch-mode-hook
 (lambda ()
 (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
 (define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
 )
 )

;; toggle whitespace-mode
(define-key global-map (kbd "C-x w") 'whitespace-mode)

(setq programming-modes (list 
			 'c-mode-common-hook
			 'python-mode-hook
			 'lisp-mode-hook
			 'lisp-interaction-hook))

(defun apply-hook-to-modes (hook-fun hook-list) (mapcar (lambda (x) (add-hook x hook-fun)) hook-list ))

(apply-hook-to-modes (function (lambda ()(local-set-key (kbd "<tab>") 'indent-or-complete))) programming-modes )
(apply-hook-to-modes (function (lambda ()
	    (font-lock-add-keywords nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
		     programming-modes )


;; kill current buffer without confirmation
(global-set-key "\C-xk" 'kill-current-buffer)

(global-unset-key "\C-x\C-v")

(defun clear-pseudo-shell ()
  (interactive)
  (erase-buffer)
  (comint-send-input)
  )

(add-hook 'shell-mode-hook (function (lambda ()(local-set-key (kbd "C-l") 'clear-pseudo-shell))))
