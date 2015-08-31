
;;; emacs --- my emacs config file

;;; Code:

;; package management
(require 'package)
;; Marmalade
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; The original ELPA archive still has some useful
;; stuff.
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))

(package-initialize)

(defvar default-packages
  '(ac-ispell auto-complete base16-theme bongo coffee-mode discover emr figlet flycheck flymake-jshint flymake-jslint flymake-php flymake-ruby flymake-yaml flymake-easy gandalf-theme hc-zenburn-theme inf-ruby key-chord less-css-mode list-utils makey markdown-mode+ markdown-toc markdown-mode mmm-mode monochrome-theme multiple-cursors noctilux-theme zenburn-theme organic-green-theme paredit pastels-on-dark-theme php+-mode php-mode popup projectile pkg-info epl redshank ruby-electric rw-hunspell s scss-mode smartparens dash web-mode yaml-mode yari ruby-block)
  "A list of packages to ensure are installed at launch.")

;; php-auto-yasnippet planet-theme purple-haze-theme seti-theme
;; soft-charcoal-theme spacegray-theme zenburn-theme  yasnippetruby-additional
;; ruby-dev ruby-refactor

(defun default-packages-installed-p ()
  (loop for p in default-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (default-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p default-packages)
    (when (not (package-installed-p p))
      (package-install p))))
;; end of package init & load

(add-to-list 'load-path "~/.emacs.d/cl-lib/")
(require 'cl-lib)
 
;; make buffer [try] to stick to current window
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode)) 

;; Go fullscreen on current frame
;; CTR + CMD + ,  = Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))

(global-set-key (kbd "C-s-,") 'toggle-fullscreen)

;; $PATH fix for internal shells
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (when window-system (set-exec-path-from-shell-PATH)) 

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
	 :channels ("#rcirc" "#emacs" "#emacswiki"))))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date))


(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'auto-complete)
(require 'yasnippet)
(require 'multiple-cursors)
(require 'php-auto-yasnippets)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

(ac-complete-yasnippet)

(setq yas-global-mode 1)

(setq ac-sources '(ac-source-semantic ac-source-yasnippet))

(setq php-auto-yasnippet-php-program "/Users/JWIJESWWW/.emacs.d/elpa/php-auto-yasnippets-20140324.1133/Create-PHP-YASnippet.php")

(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(payas/ac-setup)

;; set autosaves to temp dir

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Org Setup
(setq org-agenda-include-diary t)
(setq diary-file "~/Organiser/Diary")
(setq org-agenda-files '("~/Organiser/agendas") )

; (global-set-key "\C-cl" 'org-store-link)
 ;    (global-set-key "\C-cc" 'org-capture)
  ;   (global-set-key "\C-ca" 'org-agenda)
   ;  (global-set-key "\C-cb" 'org-iswitchb)



(put 'downcase-region 'disabled nil)


;; firefox linkage
(defun shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "/Applications/Firefox.app/Contents/MacOS/firefox -new-tab")))

(global-set-key (kbd "C-'") 'shell-command-on-buffer) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD MAPPING CHANGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; replace backspace with backwards kill word
(global-set-key (kbd "DEL") 'backward-kill-word)
(global-set-key (kbd "M-DEL") 'backward-delete-char)

; set f5 + f6 to horizontal window size. set f7 + f8 to vertical window size.
(global-set-key (kbd "<f5>") 'shrink-window-horizontally)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f7>") 'shrink-window)
(global-set-key (kbd "<f8>") 'enlarge-window)

; http://kb.mozillazine.org/Command_line_arguments
; https://developer.mozilla.org/en-US/docs/Mozilla/Command_Line_Options
; http://www.emacswiki.org/emacs/ExecuteExternalCommand

(setq ruby-insert-encoding-magic-comment nil)

;; iSpell stuff
;; (setq ispell-program-name "hunspell")
;; (require 'rw-hunspell)


(setq default-ispell-program-name "aspell")

;; EMACS Grunt Build Command
(global-set-key (kbd "<f12>") 'grunt)
(setq grunt-cmd "grunt --no-color --config ~/grunt.js")
 
(defun grunt ()
  "Run grunt"
  (interactive)
  (let* ((grunt-buffer (get-buffer-create "*grunt*"))
        (result (call-process-shell-command grunt-cmd nil grunt-buffer t))
        (output (with-current-buffer grunt-buffer (buffer-string))))
    (cond ((zerop result)
           (message "Grunt completed without errors"))
          (t
           (message nil)
           (split-window-vertically)
           (set-window-buffer (next-window) grunt-buffer)))))






;;(when (require 'some-library nil 'noerror)
 ;;e do-things)

;; inf-ruby: run irb nicely in buffers
;; (when (require 'inf-ruby nil :noerror)
;;  package-install 'inf-ruby)

(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)


;; yari: in buffer ruby documentation
;; (when (require 'yari nil :noerror)
;;  package-install 'yari)

(defun ri-bind-key ()
   (local-set-key [f1] 'yari))

(add-hook 'ruby-mode-hook 'ri-bind-key)


;; flycheck: code style checking
;; (when (require 'flycheck nil :noerror)
;;  package-install 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)


;; smartparens: better parenthasis handling & highlighting
;; (when (require 'smartparens nil :noerror)
;;  package-install 'smartparens)

(require 'smartparens-config)

(add-hook 'after-init-hook #'smartparens-global-mode)

(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
(add-hook 'prog-mode-hook 'emr-initialize)



;; ====================
;; insert date and time func

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       )

(global-set-key "\C-c\C-t" 'insert-current-date-time)
;; (global-set-key "\C-c\C-t" 'insert-current-time)

(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))


;;  in buffer build system
(setq build-buffer "*terminal*")
(setq build-cmd "vendor/bin/phpspec run\n")

(defun build-in-buffer ()
  "Send a build command to a running shell"
  (interactive)
  (process-send-string build-buffer build-cmd))

;; custom build hooks
;; use this shit to have emacs send buidl command to termnial on save file
(add-hook 'after-save-hook 'build-in-buffer)

(provide '.emacs)
;;; .emacs ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-customization-version-id 0)
 '(custom-enabled-themes (quote (evenhold)))
 '(custom-safe-themes
   (quote
    ("523d5a027e2f378ad80f9b368db450f4a5fa4a159ae11d5b66ccd78b3f5f807d" "557c715762e97e749c1c45d23f117056664dafd94465ec8c98d53e4929205a9c" "e12eca93c9766062e6ac435907a7df010f583d1c2d3c621279418a5c8f75566e" default)))
 '(one-buffer-one-frame-mode nil)
 '(package-archives
   (quote
    (("marmalade" . "http://marmalade-repo.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("elpa" . "http://tromey.com/elpa/"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(tabbar-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
