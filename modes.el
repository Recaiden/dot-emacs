;;=========================================================================;;
;;                              M O D E S                                  ;;
;;=========================================================================;;
;; Enable Normal cut, paste, copy, undo
(cua-mode)
;; Enable line numbers
(global-linum-mode)
;; Set major mode to text mode by default
(setq default-major-mode 'text-mode)
;; Turn on auto-fill-mode by default in unadorned files.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; Display the column numer in the mode line
(setq column-number-mode t)

(setq auto-mode-alist
      (append '(("\\.C$"   . c++-mode)
                ("\\.cc$"  . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.cxx$" . c++-mode)
                ("\\.hxx$" . c++-mode)
                ("\\.h$"   . c++-mode)
                ("\\.hh$"  . c++-mode)
                ("\\.idl$" . c++-mode)
                ("\\.c$"   . c-mode)
                ("\\.pl$" . perl-mode) 
                ("\\.pm$" . perl-mode)
                ("\\.java$" . java-mode)
		("\\.as$" . javascript-mode)
		("\\.json$" . javascript-mode)
		("\\.emacs$" . lisp-mode)
		("\\Rakefile$" . ruby-mode)
		("\\.qss$" . css-mode)
                ("\\.txt$" . text-mode))
              auto-mode-alist))

;;; Show matching parens
(require 'paren)

;; Universal Lambda
(require 'pretty-lambda)
(require 'pretty-delta)
(global-pretty-delta-mode)
(global-pretty-lambda-mode)

;; Thanks to Javier Oviedo for the paren matching and err case system
;; Automatically makes the matching paren stand out in color.
(condition-case err
    (show-paren-mode t)
  (error
   (message "Cannot show parens %s" (cdr err))))
