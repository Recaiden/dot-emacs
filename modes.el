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

(setq-default typescript-indent-level 2)

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
		("\\.ts"   . typescript-mode)
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

;; ts lint
(add-hook 'after-init-hook #'global-flycheck-mode)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Universal Lambda - deprecated in 24.4
;(require 'pretty-lambda)
;(require 'pretty-delta)
;(global-pretty-delta-mode)
;(global-pretty-lambda-mode)

(global-prettify-symbols-mode 1)
(defun my-add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
	  ("delta" . 916) ; Δ
          ("lambda" . 955) ; λ
          ("->" . 8594)    ; →
          ("=>" . 8658)    ; ⇒
					;("map" . 8614)   ; ↦

	  ;comparators
	  ("<=" .      #x2264) ; ≤ 
	  (">=" .      #x2265) ; ≥ 

	  
	  ("def" .      #x2131) ; ℱ
	  ("func" .      #x2131) ; ℱ
	  ("fun" .      #x2131) ; ℱ

	  ("not" .      #x2757) ; ❗
	  ("in" .       #x2208) ; ∈
	  ("not in" .   #x2209) ; ∉
	  ("return" .   #x23ce) ; ⏎
	  ("yield" .    #x26db) ; ⛛
	  ("for" .      #x2200) ; ∀
	  ;; Base Types
	  ("int" .      #x2124) ; ℤ
	  ("float" .    #x211d) ; ℝ
	  ("str" .      #x1d54a) ; 𝕊
	  ("True" .     #x1d54b) ; 𝕋
	  ("False" .    #x1d53d) ; 𝔽  
	  ;; Mypy
	  ("Dict" .     #x1d507) ; 𝔇
	  ("List" .     #x2112) ; ℒ
	  ;("Tuple" .    #x2a02) ; ⨂
	  ("Set" .      #x2126) ; Ω
	  ("Iterable" . #x2941) ; ⥁
	  ("Any" .      #x2754) ; ❔
	  ("Union" .    #x22c3) ; ⋃ 
          )))
;(setq prettify-symbols-unprettify-at-point t)
(add-hook 'prog-mode-hook 'my-add-pretty-lambda)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Thanks to Javier Oviedo for the paren matching and err case system
;; Automatically makes the matching paren stand out in color.
(condition-case err
    (show-paren-mode t)
  (error
   (message "Cannot show parens %s" (cdr err))))
