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

(global-unset-key "\C-a")
(define-key global-map "\C-a" 'mark-whole-buffer)


(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Make Control and Page Up/Page Down scroll the other buffer
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)

;; Tab through buffers
(global-set-key (kbd"<C-tab>") 'next-buffer)
(global-set-key (kbd"<C-S-tab>") 'previous-buffer)
(global-set-key (kbd"<C-S-iso-lefttab>") 'previous-buffer)

;; Make search use Ctrl+f and Ctrl+Shift+f for reverse like a reasonable app
(define-key global-map (kbd "C-f") 'isearch-forward)
(define-key global-map (kbd "C-S-f") 'isearch-backward)
(add-hook 'isearch-mode-hook
 (lambda ()
 (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
 (define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
 )
)

(add-hook 'c-mode-common-hook (function (lambda ()(local-set-key (kbd "<tab>") 'indent-or-complete))))
(add-hook 'python-mode-hook (function (lambda () (local-set-key (kbd "<tab>") 'indent-or-complete))))
(add-hook 'lisp-mode-hook (function (lambda () (local-set-key (kbd "<tab>") 'indent-or-complete))))
(add-hook 'lisp-interaction-hook (function (lambda () (local-set-key (kbd "<tab>") 'indent-or-complete))))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
(add-hook 'python-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; kill current buffer without confirmation
(global-set-key "\C-xk" 'kill-current-buffer)

(global-unset-key "\C-x\C-v")

(defun clear-pseudo-shell ()
  (erase-buffer)
  (comint-send-input)
  )

(add-hook 'shell-mode-hook (function (lambda ()(local-set-key (kbd "C-l") 'clear-pseudo-shell))))