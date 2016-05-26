;; This is modified from SR Borrett's emacs customization file on HERA - 21 July 2011
;; hermes
;; ========LAST MODIFIED 30Mar2015========

;; NOTE use this to find help: C-h i m emacs RET


;;The path where you store your *.el files
(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(if (< emacs-major-version 23)
(defun characterp (obj)
(and (char-or-string-p obj) (not (stringp obj)))))


;;MELPA - package loader
;; From within emacs run 'package-list-packages'
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;ess
(require 'ess-site)
(setq ess-history-directory "~/.R/")

;; markdown-mode.el
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rmd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

;; tangotango color there
(load-theme 'tangotango t) 

;; Keyboard shortcuts
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)

;; Hiding backup files (~ and #)
;; Don't clutter up directories with files~
;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Default enter winner mode
(winner-mode 1)

;; Disable ‘C-x C-c’

    (defun dont-kill-emacs ()
      (interactive)
      (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
  
    (global-set-key "\C-x\C-c" 'dont-kill-emacs)
;;    (global-set-key "\C-z" 'shell)

;; Define previous other window
(global-set-key "\C-xp" (lambda ()
			  (interactive)
			  (other-window -1)))

;; Disable ‘C-[ C-[ C-[’

    (defun dont-escape-out ()
      (interactive)
      (error (substitute-command-keys "Escape out disabled. See ~/.emacs Disable 'C-[ C-[ C-['.")))
  
    (global-set-key "\C-[\C-[\C-[" 'dont-escape-out)


;; add date and time to status bar
(setq display-time-day-and-date t
                display-time-24hr-format t)
             (display-time)

;;Turn off bell
(setq ring-bell-function 'ignore)

;;Change error message from beep to visual
(setq ring-bell-function (lambda () (message "***What the f$%k!***")))

;;optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

;; Add local lisp folder to load-path                                              
(setq load-path (append load-path (list "~/elisp")))                               

;; flyspell ----                                                                   
(autoload 'flyspell-mode-on "flyspell" "On-the-fly ispell." t)

;;; TEXT MODE and Auto Fill                                                        
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq colon-double-space t)
;(add-hook 'text-mode-hook 'flyspell-mode)

;; LATEX MODIFICATIONS                                                             
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)


;; turn on paraenthesis highlighting                                               
(require 'paren)
(show-paren-mode 1)

                                                                                   
;; Syntax highlighting:
(global-font-lock-mode t)                                
(transient-mark-mode t)


(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

;; polymode for RMarkdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rmd\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'turn-on-outline-minor-mode)

(defun rmarkdown-new-chunk (name)
  "Insert a new R chunk."
  (interactive "sChunk name: ")
  (insert "\n```{r " name "}\n")
  (save-excursion
    (newline)
    (insert "```\n")
    (previous-line)))

(defun rmarkdown-weave-file ()
  "Run knitr on the current file and weave it as MD and HTML."
  (interactive)
  (shell-command
   (format "knit.sh -c %s"
	   (shell-quote-argument (buffer-file-name)))))

(defun rmarkdown-tangle-file ()
  "Run knitr on the current file and tangle its R code."
  (interactive)
  (shell-command
   (format "knit.sh -t %s"
	   (shell-quote-argument (buffer-file-name)))))

(defun rmarkdown-preview-file ()
  "Run knitr on the current file and display output in a browser."
  (interactive)
  (shell-command
   (format "knit.sh -b %s"
	          (shell-quote-argument (buffer-file-name)))))


(defun ess-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
					; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
	     (sbuffer (process-buffer sprocess))
	     (buf-coding (symbol-name buffer-file-coding-system))
	     (R-cmd
	      (format "library(rmarkdown); rmarkdown::render(\"%s\")"
		      buffer-file-name)))
	(message "Running rmarkdown on %s" buffer-file-name)
	(ess-execute R-cmd 'buffer nil nil)
	(switch-to-buffer rmd-buf)
	         (ess-show-buffer (buffer-name sbuffer) nil)))))

;; saving keyboard macros
(defun save-macro (name)                  
    "save a macro. Take a name as argument
     and save the last defined macro under 
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro    
     (kmacro-name-last-macro name)         ; use this name for the macro    
     (find-file user-init-file)            ; open ~/.emacs or other user init file 
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro 
     (newline)                             ; insert a newline
     (switch-to-buffer nil))               ; return to the initial buffer

;;;keyboard macros
(fset 'copy-text-ansi
   (lambda (&optional arg) "Keyboard macro." (interactive "p") 
     (kmacro-exec-ring-item (
			     quote (
				    "oo" 
				    0 "%d")) arg)))

(fset 'editor
   (lambda (&optional arg) "Keyboard
   macro." (interactive "p") (kmacro-exec-ring-item 
			      (quote (
				      "3315{oo55{oo25^"
				      0 "%d")) arg)))

;; render ESS Rmarkdown
(global-set-key (kbd "C-c er") 'ess-rmarkdown)  
